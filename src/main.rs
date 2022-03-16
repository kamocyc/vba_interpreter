#![feature(try_blocks)]
#![allow(unused_braces)]
#![allow(unused_parens)]

mod gen;

mod native {
  pub mod debug {
    pub fn print(arguments: &Vec<crate::runtime::Value>)-> crate::runtime::Value {
      println!("printed: {:?}", arguments[0]);
      crate::runtime::Value::Int(0)
    }
  }
}

mod runtime;

use std::collections::HashMap;
use std::rc::Rc;
use std::env;

macro_rules! hashmap {
  ($( $key: expr => $val: expr ),*) => {{
    let mut map = ::std::collections::HashMap::new();
    $( map.insert($key, $val); )*
    map
  }}
}

fn prepare_global_env()-> crate::runtime::Env {
  crate::runtime::Env::new(
    vec![
      (Rc::new("Debug".to_owned()),
      crate::runtime::Value::Object(Rc::new(crate::runtime::Object {
        fields: HashMap::new(),
        methods: hashmap![Rc::new("Print".to_owned()) => crate::runtime::Function{
            id: Rc::new("Print".to_owned()),
            parameters: vec![crate::runtime::Parameter {name: Rc::new("message".to_owned()), typename: crate::gen::ast::Typename::Variant}],
            body: crate::runtime::FunctionBody::Native(crate::native::debug::print)
          }]
      }))),
      // ("CStr".to_owned(),
      // crate::runtime::Value::Function(
      //   id: "CStr".to_owned(),
      //   parameters: vec![Parameter {name: "n".to_owned(), typename: crate::gen::ast::Typename::Integer}]
      // )
      // )
    ]
  )
}

fn add_global_env(env: &mut crate::runtime::Env, module: crate::gen::ast::Module) {
  let mut functions = HashMap::new();
  for function in module.functions {
    functions.insert(Rc::clone(&function.name), crate::runtime::Value::Function(Rc::new(crate::runtime::Function::new(function))));
  }
  
  env.append_global(functions);
}

fn main() {
  // 最初に読み込んだファイルのmain関数をエントリポイントとする。
  let args: Vec<String> = env::args().collect();
  let mut global_env = prepare_global_env();
  for (i, arg) in args.iter().enumerate() {
    if i >= 1 {
      println!("{}", &arg);
      let module = crate::gen::parse(&arg);
      add_global_env(&mut global_env, module);
    }
  }
    
  let prog = crate::runtime::Program::new();
  let result = prog.evaluate_program(&mut global_env, Rc::new("main".to_owned()));
  
  println!("result: {:?}", result);
}
