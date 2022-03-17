use crate::gen::ast::*;
use std::rc::Rc;

pub fn check_module(module: &Module, global_env: &crate::runtime::Env,option_explict: bool) {
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
    module.functions.iter().map(|f| Rc::clone(&f.name)).collect();
  {
    // 識別子の重複検査
    fn get_duplicate<'a>(vec: &Vec<Id>, eq: (fn(&Id, &Id) -> bool))-> Option<Id> {
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
    
    match get_duplicate(&function_names, |n1, n2| n1 == n2) {
      Some(d) => panic!("duplicate function name {}", d),
      None => ()
    }
  }
  
  let global_names: Vec<Id> =
    global_env.get_global_names().into_iter().chain(function_names).collect();
  
  for function in module.functions.iter() {
    let mut found_declarations = function.parameters.iter().map(|a| Rc::clone(&a.name)).collect::<Vec<Id>>();
    for statement in function.body.statements.iter() {
      match statement {
        Statement::VariableDeclaration(VariableDeclaration {name, ..}) => {
          // 変数が既に宣言されている
          if found_declarations.contains(name) {
            panic!("duplicate variable declaration {}", name);
          }
          found_declarations.push(Rc::clone(name));
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
            if !found_declarations.contains(variable) && !global_names.contains(variable) {
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
}
