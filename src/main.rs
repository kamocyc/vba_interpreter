#![feature(try_blocks)]
#![allow(unused_braces)]
#![allow(unused_parens)]

mod gen;
mod runtime;
mod static_checker;

mod native {
  use crate::runtime::Value;
  
  pub mod debug {
    use crate::runtime::Value;
    
    pub fn print(arguments: &Vec<Value>)-> Value {
      println!("printed: {:?}", arguments[0]);
      Value::Int(0)
    }
  }
  
  pub fn msgbox(arguments: &Vec<Value>)-> Value {
    println!("msgbox: {:?}", arguments[0]);
    Value::Int(0)
  }
  
  pub fn inputbox(arguments: &Vec<Value>)-> Value {
    println!("{:?}", arguments[0]);
    let mut s = String::new();
    std::io::stdin().read_line(&mut s).ok();
    Value::String(std::rc::Rc::new(s))
  }
}

mod prepare {
  use std::collections::HashMap;
  use std::rc::Rc;
  use crate::gen::ast::Typename;
  use crate::runtime::{Env, Value, Object, Function, Parameter, FunctionBody};
  // use crate::runtime::*;
  // use crate::runtime::Function;

  macro_rules! hashmap {
    ($( $key: expr => $val: expr ),*) => {{
      let mut map = ::std::collections::HashMap::new();
      $( map.insert($key, $val); )*
      map
    }}
  }

  fn create_function_pair(name: &str, parameters: Vec<(&str, Typename)>, body: fn(&Vec<crate::runtime::Value>) -> crate::runtime::Value)-> (Rc<String>, Value) {
    (
      Rc::new(name.to_owned()),
      Value::Function(
        Rc::new(Function {
          id: Rc::new(name.to_owned()),
          parameters: parameters.iter().map(|(name, typname)| Parameter {name: Rc::new(name.to_string()), typename: typname.clone()}).collect(),
          body: FunctionBody::Native(body),
        })
      )
    )
  }
  
  pub fn prepare_global_env()-> Env {
    Env::new(
      vec![
        (
          Rc::new("Debug".to_owned()),
          Value::Object(Rc::new(Object {
            fields: HashMap::new(),
            methods: hashmap![Rc::new("Print".to_owned()) => Function{
                id: Rc::new("Print".to_owned()),
                parameters: vec![Parameter {name: Rc::new("message".to_owned()), typename: Typename::Variant}],
                body: FunctionBody::Native(crate::native::debug::print)
              }]
          }))
        ),
        create_function_pair("MsgBox", vec![("Prompt", Typename::String)], crate::native::msgbox),
        create_function_pair("InputBox", vec![("Prompt", Typename::String)], crate::native::inputbox),
        
        // ("CStr".to_owned(),
        // Value::Function(
        //   id: "CStr".to_owned(),
        //   parameters: vec![Parameter {name: "n".to_owned(), typename: Typename::Integer}]
        // )
        // )
      ]
    )
  }

  pub fn add_global_env(env: &mut Env, module: crate::gen::ast::Module) {
    let mut functions = HashMap::new();
    for function in module.functions {
      functions.insert(Rc::clone(&function.name), Value::Function(Rc::new(Function::new(function))));
    }
    
    env.append_global(functions);
  }
}

use std::env;
use std::rc::Rc;

fn main() {
  // 最初に読み込んだファイルのmain関数をエントリポイントとする。
  let args: Vec<String> = env::args().collect();
  let mut global_env = prepare::prepare_global_env();
  for (i, arg) in args.iter().enumerate() {
    if i >= 1 {
      println!("{}", &arg);
      let module = crate::gen::parse(&arg);
      let option_explicit = true;
      crate::static_checker::check_module(&module, &global_env, option_explicit);
      prepare::add_global_env(&mut global_env, module);
    }
  }
    
  let prog = crate::runtime::Program::new();
  let result = prog.evaluate_program(&mut global_env, Rc::new("main".to_owned()));
  
  println!("result: {:?}", result);
}
