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

macro_rules! hashmap {
  ($( $key: expr => $val: expr ),*) => {{
    let mut map = ::std::collections::HashMap::new();
    $( map.insert($key, $val); )*
    map
  }}
}

fn main() {
  let filename = "test/test1.bas";
  let module = crate::gen::parse(filename);
  
  let mut global_env =
    crate::runtime::Env::new(
      vec![
        ("Debug".to_owned(),
        crate::runtime::Value::Object(Rc::new(crate::runtime::Object {
          fields: HashMap::new(),
          methods: hashmap!["Print".to_owned() => crate::gen::ast::Function{
              id: "Print".to_owned(),
              parameters: vec!["message".to_owned()],
              body: crate::gen::ast::FunctionBody::Native(crate::native::debug::print)
            }]
        }))),
      ]
    );
    
  let prog = crate::runtime::Program::new();
  let result = prog.evaluate_program(module, &mut global_env, &"main".to_owned());
  
  println!("result: {:?}", result);
}
