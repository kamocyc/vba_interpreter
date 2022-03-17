#![feature(try_blocks)]
#![allow(unused_braces)]
#![allow(unused_parens)]

mod gen;
mod runtime;
mod static_checker;

mod native {
    use crate::runtime::{Env, Value, cast_to_string};
  
  pub mod debug {
    use crate::runtime::{Env, Value};
    
    pub fn print(_env: &Env, arguments: &Vec<Value>)-> Value {
      println!("{:?}", arguments[0]);
      Value::Integer(0)
    }
  }
  
  pub fn msgbox(_env: &Env, arguments: &Vec<Value>)-> Value {
      println!("{}", cast_to_string(arguments[0].clone()));
    Value::Integer(0)
  }
  
  pub fn inputbox(_env: &Env, arguments: &Vec<Value>)-> Value {
    println!("{}", cast_to_string(arguments[0].clone()));
    let mut s = String::new();
    std::io::stdin().read_line(&mut s).ok();
    Value::String(std::rc::Rc::new(s))
  }
}

mod prepare {
  use std::collections::HashMap;
  use std::rc::Rc;
  use crate::gen::ast::Typename;
  use crate::runtime::{Env, Value, Object, Function, Parameter, FunctionBody, Definition};
  use std::cell::RefCell;
  // use crate::runtime::*;
  // use crate::runtime::Function;

  macro_rules! hashmap {
    ($( $key: expr => $val: expr ),*) => {{
      let mut map = ::std::collections::HashMap::new();
      $( map.insert($key, $val); )*
      map
    }}
  }

  fn create_function_pair(name: &str, parameters: Vec<(&str, Typename)>, body: fn(&Env, &Vec<crate::runtime::Value>) -> crate::runtime::Value)-> (Rc<String>, Value) {
    (
      Rc::new(name.to_owned()),
      Value::Definition(Definition::Function(
        Rc::new(Function {
          id: Rc::new(name.to_owned()),
          parameters: parameters.iter().map(|(name, typname)| Parameter {name: Rc::new(name.to_string()), typename: typname.clone()}).collect(),
          body: FunctionBody::Native(Box::new(body)),
        })
      )
    ))
  }
  
  fn tw(s: &str)-> Rc<String> {
    Rc::new(s.to_owned())
  }
  
  fn create_range_object(range: crate::runtime::Range)-> Rc<Object> {
    Rc::new(Object {
      fields: RefCell::new(hashmap![
        tw("StartRow") => Value::Integer(range.start_row),
        tw("StartColumn") => Value::Integer(range.start_col),
        tw("EndRow") => Value::Integer(range.end_row),
        tw("EndColumn") => Value::Integer(range.end_col)
      ]),
      default_property: Some(tw("Value")),
      source_module: None,
      methods: hashmap![
        tw("Value") => Rc::new(Function {
          id: tw("Value"),
          parameters: vec![],
          body: FunctionBody::Native(Box::new(|env: &Env, _arguments: &Vec<Value>| {
            let row = crate::runtime::cast_to_int(env.get_field_value_opt(&tw("StartRow")).unwrap());
            let col = crate::runtime::cast_to_int(env.get_field_value_opt(&tw("StartColumn")).unwrap());
            let worksheet = &env.get_system().worksheet;
            let range = crate::runtime::Worksheet::get_range(Rc::clone(worksheet), row, col);
            range.get_value()
          }))
        })
      ],
      setters: hashmap![
        tw("Value") => Rc::new(Function {
          id: tw("Value"),
          parameters: vec![Parameter {name: tw("value"), typename: Typename::Variant}],
          body: FunctionBody::Native(Box::new(|env: &Env, arguments: &Vec<Value>| {
            let row = crate::runtime::cast_to_int(env.get_field_value_opt(&tw("StartRow")).unwrap());
            let col = crate::runtime::cast_to_int(env.get_field_value_opt(&tw("StartColumn")).unwrap());
            let worksheet = &env.get_system().worksheet;
            let mut range = crate::runtime::Worksheet::get_range(Rc::clone(worksheet), row, col);
            range.set_value(arguments[0].clone());
            Value::Integer(0)
          }))
        })
      ],
    })
  }
  
  pub fn prepare_global_env()-> Env {
    let worksheet = Rc::new(std::cell::RefCell::new(crate::runtime::Worksheet::new()));
    let worksheet = crate::runtime::System { worksheet };
    
    fn to_range(address: &str)-> crate::runtime::Range {
      fn to_row_and_column(address: &str)-> (i32, i32) {
        let (column, number_start_index) = {
          let i0 = address.chars().nth(0).unwrap();
          let i1 = address.chars().nth(1).unwrap();
          let i2 = match address.chars().nth(2) { Some(c) => c, None => ' ' };
          if i0.is_ascii_alphabetic() {
            if i1.is_ascii_alphabetic() {
              if i2.is_ascii_alphabetic() {
                // 3 digits
                ((i2 as i32 - 'A' as i32) * 26 * 26 + (i1 as i32  - 'A' as i32) * 26 + (i0 as i32  - 'A' as i32), 3 as usize)
              } else {
                // 2 digits
                ((i1 as i32  - 'A' as i32) * 26 + (i0 as i32  - 'A' as i32), 2 as usize)
              }
            } else {
              // 1 digit
              ((i0 as i32  - 'A' as i32), 1 as usize)
            }
          } else {
            panic!("illegal address {:?}", i0);
          }
        };
        
        let mut row = 0;
        for c in address.chars().skip(number_start_index) {
          row = row * 10 + c as i32 - '0' as i32;
        }
        (row, column + 1)
      }
      match address.find(':') {
        Some(index) => {
          let (start, end) = address.split_at(index);
          let end = end.trim_start_matches(':');
          
          let start = to_row_and_column(start);
          let end = to_row_and_column(end);
          crate::runtime::Range {
            start_row: start.0,
            start_col: start.1,
            end_row: end.0,
            end_col: end.1,
          }
        },
        None => {
          let (row, column) = to_row_and_column(address);
          crate::runtime::Range {
            start_row: row,
            start_col: column,
            end_row: row,
            end_col: column,
          }
        }
      }
    }
    
    Env::new(
      worksheet,
      vec![
        (
          Rc::new("Debug".to_owned()),
          Value::Object(Rc::new(Object {
            fields: RefCell::new(HashMap::new()),
            default_property: None,
            source_module: None,
            methods: hashmap![Rc::new("Print".to_owned()) => Rc::new(Function{
                id: Rc::new("Print".to_owned()),
                parameters: vec![Parameter {name: Rc::new("message".to_owned()), typename: Typename::Variant}],
                body: FunctionBody::Native(Box::new(crate::native::debug::print))
              })],
            setters: HashMap::new(),
          }))
        ),
        (
          tw("Cells"),
          Value::Object(Rc::new(Object {
            fields: RefCell::new(HashMap::new()),
            default_property: Some(tw("Item")),
            source_module: None,
            methods: hashmap![
              tw("Item") => Rc::new(Function{
                id: tw("Item"),
                parameters: vec![Parameter {name: tw("RowIndex"), typename: Typename::Integer}, Parameter {name: tw("ColumnIndex"), typename: Typename::Integer}],
                body: FunctionBody::Native(Box::new(|_env: &Env, arguments: &Vec<Value>| {
                  let row = crate::runtime::cast_to_int(arguments[0].clone());
                  let col = crate::runtime::cast_to_int(arguments[1].clone());
                  Value::Object(create_range_object(crate::runtime::Range::new_singleton(row, col)))
                  // let worksheet = &env.get_system().worksheet;
                  // let range = crate::runtime::Worksheet::get_range(Rc::clone(worksheet), row, col);
                  // range.get_value()
                }))
              }),
              tw("Range") => Rc::new(Function{
                id: tw("Range"),
                parameters: vec![Parameter {name: tw("Cell1"), typename: Typename::Variant}, Parameter {name: tw("Cell2"), typename: Typename::Variant}],
                body: FunctionBody::Native(Box::new(|_env: &Env, arguments: &Vec<Value>| {
                  match &arguments[0] {
                    Value::String(s) => {
                      let range = to_range(&s);
                      let range = create_range_object(range);
                      
                      Value::Object(range)
                    },
                    _ => panic!("illegal argument")
                  }
                }))
              })
            ],
            setters: HashMap::new(),
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

  pub fn add_global_env(env: &mut Env, module_name: &str, module_type: crate::runtime::ModuleType, module: crate::gen::ast::Module) {
    let mut functions = HashMap::new();
    let mut setters = HashMap::new();
    for function in module.functions {
      match function.function_type {
        crate::gen::ast::FunctionType::Function | crate::gen::ast::FunctionType::Procedure => {
          functions.insert(Rc::clone(&function.name), Rc::new(Function::new(function)));
        },
        crate::gen::ast::FunctionType::PropertyLet => {
          setters.insert(Rc::clone(&function.name), Rc::new(Function::new(function)));
        },
      }
    }
    
    let mut fields = HashMap::new();
    for field in module.fields {
      fields.insert(Rc::clone(&field.name), field.typename);
    }
    
    let module =
      crate::runtime::Module {
        name: Rc::new(module_name.to_owned()),
        functions: functions,
        fields: fields,
        setters: setters,
        module_type: module_type,
      };
    
    env.append_global(hashmap![
      tw(module_name) => Value::Definition(Definition::Module(Rc::new(module)))
    ]);
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
      println!("input file: {}", &arg);
      let module = crate::gen::parse(&arg);
      let base_name = arg.split('/').last().unwrap().split('.').next().unwrap();
      let module_type =
        match arg.split('.').last().unwrap() {
          "bas" => crate::runtime::ModuleType::Module,
          "cls" => crate::runtime::ModuleType::Class,
          _ => panic!("unknown file type"),
        };
      let option_explicit = true;
      // crate::static_checker::check_module(&module, &global_env, option_explicit);
      prepare::add_global_env(&mut global_env, base_name, module_type, module);
    }
  }
  
  let t = "test1".to_owned();
  let prog = crate::runtime::Program::new();
  let (result, env) = prog.evaluate_program(&mut global_env, Rc::new(t), Rc::new("main".to_owned()));
  
  println!("result: {:?}", result);
  
  (*env.get_system().worksheet).borrow().print_worksheet();
}
