use crate::gen::ast::*;
use std::rc::Rc;
use crate::runtime::{Env, Module, Id, Function, FunctionBody};

pub fn check_module(module: &Module, global_env: &Env) {
  // ここで行うのは識別子のチェック
  // とりあえず、モジュールレベルでの識別子、プロシージャレベルの識別子 をチェックする
  // 各識別子の型を取得, 使われ方があっていることを確認, 2-passとする
  /* 関数:
    引数の数が合っている
    同名の関数が定義されていない
    プロシージャに返り値が代入されていない
  */
  /*
    変数:
    （byref渡しで型が合っている、）2重に定義されていない、dimの前に代入は宣言の重複。
    （TODO: moduleレベルと識別子が重なる場合は、ローカルを優先）
    option explicitがある場合、dimなしでの変数利用は禁止（任意のsubexpressionについて）
    （めんどい）
  */
  /*
    値の暗黙的な変換をする。
  */
  
  let function_names: Vec<Id> =
    module.functions.iter().map(|(n, _)| Rc::clone(&n))
    .chain(
      module.setters.iter().map(|(n, _)| Rc::clone(&n))
    ).collect();
  
  let field_names: Vec<Id> =
    module.fields.iter().map(|(n, _)| Rc::clone(&n)).collect();
  
  let module_level_names = function_names 
    .iter()
    .chain(field_names.iter())
    .map(|id| Rc::clone(id))
    .collect();
  {
    // モジュールレベルの識別子の重複検査
    fn get_duplicate_id<'a>(vec: &Vec<Id>, eq: (fn(&Id, &Id) -> bool))-> Option<Id> {
      let mut found = None;
      for (i, val) in vec.iter().enumerate() {
        for j in (i + 1)..(vec.len()) {
          if eq(val, vec.get(j).unwrap()) {
            found = Some(val.clone());
          }
        }
      }
      found
    }
    
    match get_duplicate_id(&module_level_names, |n1, n2| n1 == n2) {
      Some(d) => panic!("duplicate name {}", d),
      None => ()
    }
  }
  
  let global_names: Vec<Id> =
    global_env.get_global_names().into_iter().chain(function_names).collect();
  
  // 関数内の識別子宣言の重複検査
  // option_explicit有効時は、宣言されていない変数を検査
  fn check_variable_declarations(function: &Function, outer_scope_names: &Vec<Id>, option_explict: bool) {
    let mut found_declarations = function.parameters.iter().map(|a| Rc::clone(&a.name)).collect::<Vec<Id>>();
    match &function.body {
      FunctionBody::VBA(function_body) => {
        for statement in function_body.statements.iter() {
          match statement {
            Statement::VariableDeclaration(VariableDeclaration {name, ..}) => {
              // 変数が既に宣言されている
              if found_declarations.contains(&name) {
                panic!("duplicate variable declaration {}", name);
              }
              found_declarations.push(Rc::clone(&name));
            },
            Statement::Assign(_, Chain::App(App { name, .. }), _) => {
              if *name == function.name && function.function_type == FunctionType::Procedure {
                panic!("a return value cannot be assigned to the procedure {}", name);
              }
            }
            _ => {
              let variables = statement.get_variables();
              // 未宣言の変数が出現
              for variable in variables.iter() {
                if !found_declarations.contains(variable) && !outer_scope_names.contains(variable) {
                  if option_explict {
                    panic!("undeclared variable \"{}\"", variable);
                  } else {
                    found_declarations.push(Rc::clone(variable));
                  }
                }
              }
            }
          }
        }
      }
      FunctionBody::Native(_) => (),
    }
  }
  
  let outer_scope_names =
    global_names.iter().chain(module_level_names.iter()).map(|id| Rc::clone(id)).collect();
  for (_, function) in module.functions.iter() {
    check_variable_declarations(&function, &outer_scope_names, module.option_explicit);
  }
}

pub fn check(global_env: &Env) {
  // モジュール内の識別子の出現、重複の検査
  // 他モジュールや型のメンバの有効性検査
  
  for module in global_env.get_vba_modules() {
    check_module(module.as_ref(), global_env);
  }
}
