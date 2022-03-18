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
  use crate::runtime::{Env, Value, Object, Function, Parameter, FunctionBody, Definition, param};
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

  fn create_function(name: &str, parameters: Vec<Parameter>, body: fn(&Env, &Vec<crate::runtime::Value>) -> crate::runtime::Value, return_typename: Typename)-> Function {
    Function {
      name: Rc::new(name.to_owned()),
      modifier: crate::gen::ast::Modifier::Public,
      function_type: crate::gen::ast::FunctionType::Function,
      return_typename: return_typename,
      parameters: parameters,
      body: FunctionBody::Native(Box::new(body)),
    }
  }
  
  fn create_function_pair(name: &str, parameters: Vec<(Parameter)>, body: fn(&Env, &Vec<crate::runtime::Value>) -> crate::runtime::Value, return_typename: Typename)-> (Rc<String>, Value) {
    (
      Rc::new(name.to_owned()),
      Value::Definition(Definition::Function(Rc::new(
        create_function(name, parameters, body, return_typename)
      )))
    )
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
        tw("Value") => Rc::new(
          create_function(
            "Value",
            vec![],
            |env: &Env, _arguments: &Vec<Value>| {
              let row = crate::runtime::cast_to_int(env.get_field_value_opt(&tw("StartRow")).unwrap());
              let col = crate::runtime::cast_to_int(env.get_field_value_opt(&tw("StartColumn")).unwrap());
              let worksheet = &env.get_system().worksheet;
              let range = crate::runtime::Worksheet::get_range(Rc::clone(worksheet), row, col);
              range.get_value()
            },
            Typename::Variant
          )
        )
      ],
      setters: hashmap![
        tw("Value") => Rc::new(
          create_function(
            "Value",
            vec![param("value", Typename::Variant)],
            |env: &Env, arguments: &Vec<Value>| {
              let row = crate::runtime::cast_to_int(env.get_field_value_opt(&tw("StartRow")).unwrap());
              let col = crate::runtime::cast_to_int(env.get_field_value_opt(&tw("StartColumn")).unwrap());
              let worksheet = &env.get_system().worksheet;
              let mut range = crate::runtime::Worksheet::get_range(Rc::clone(worksheet), row, col);
              range.set_value(arguments[0].clone());
              Value::Integer(0)
            },
            Typename::Variant
          )
         )
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
          tw("Array"),
          Value::Definition(Definition::Function(Rc::new(
            create_function(
              "Array",
              vec![
                Parameter {
                  name: tw("ArgList"),
                  typename: Typename::Variant,
                  is_optional: true,
                  is_param_array: true,
                }
              ],
              |_env: &Env, arguments: &Vec<Value>| {
                let mut array = crate::runtime::Array::new(vec![]);
                for argument in arguments {
                  array.data.push(argument.clone());
                }
                Value::Array(Rc::new(RefCell::new(array)))
              },
              Typename::Variant)
          )))
        ),
        (
          tw("UBound"),
          Value::Definition(Definition::Function(Rc::new(
            create_function(
              "UBound",
              vec![param("array", Typename::Variant)],
              |_env: &Env, arguments: &Vec<Value>| {
                match &arguments[0] {
                  Value::Array(array) => {
                    let array = array.borrow();
                    Value::Integer(array.data.len() as i32 - 1)
                  },
                  _ => panic!("illegal argument type")
                }
              },
              Typename::Integer
            )
          )))
        ),
        (
          tw("LBound"),
          Value::Definition(Definition::Function(Rc::new(
            create_function(
              "LBound",
              vec![param("array", Typename::Variant)],
              |_env: &Env, arguments: &Vec<Value>| {
                match &arguments[0] {
                  Value::Array(_) => {
                    Value::Integer(0)
                  },
                  _ => panic!("illegal argument type")
                }
              },
              Typename::Integer
            )
          )))
        ),
        (
          Rc::new("Debug".to_owned()),
          Value::Object(Rc::new(Object {
            fields: RefCell::new(HashMap::new()),
            default_property: None,
            source_module: None,
            methods: hashmap![Rc::new("Print".to_owned()) => Rc::new(
              create_function(
                "Print",
                vec![param("message", Typename::Variant)],
                crate::native::debug::print,
                Typename::Variant
              )
            )],
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
              tw("Item") => Rc::new(
                create_function(
                  "Item",
                  vec![param("RowIndex", Typename::Integer), param("ColumnIndex", Typename::Integer)],
                  |_env: &Env, arguments: &Vec<Value>| {
                    let row = crate::runtime::cast_to_int(arguments[0].clone());
                    let col = crate::runtime::cast_to_int(arguments[1].clone());
                    Value::Object(create_range_object(crate::runtime::Range::new_singleton(row, col)))
                    // let worksheet = &env.get_system().worksheet;
                    // let range = crate::runtime::Worksheet::get_range(Rc::clone(worksheet), row, col);
                    // range.get_value()
                  },
                  Typename::Id(tw("Range"))
                )
              ),
              tw("Range") => Rc::new(
                create_function(
                  "Range",
                  vec![
                    param("Cell1", Typename::Variant),
                    Parameter {
                      name: tw("Cell2"),
                      typename: Typename::Variant,
                      is_optional: true,
                      is_param_array: false,
                    }
                  ],
                  |_env: &Env, arguments: &Vec<Value>| {
                    match &arguments[0] {
                      Value::String(s) => {
                        let range = to_range(&s);
                        let range = create_range_object(range);
                        
                        Value::Object(range)
                      },
                      _ => panic!("illegal argument")
                    }
                  },
                  Typename::Id(tw("Range"))
                )
              )
            ],
            setters: HashMap::new(),
          }))
        ),
        create_function_pair("MsgBox", vec![param("Prompt", Typename::String)], crate::native::msgbox, Typename::Integer),
        create_function_pair("InputBox", vec![param("Prompt", Typename::String)], crate::native::inputbox, Typename::String),
        
        // ("CStr".to_owned(),
        // Value::Function(
        //   id: "CStr".to_owned(),
        //   parameters: vec![Parameter {name: "n".to_owned(), typename: Typename::Integer}]
        // )
        // )
      ]
    )
  }

  fn build_module(module_name: &str, module_type: crate::runtime::ModuleType, module: crate::gen::ast::Module)-> crate::runtime::Module {
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
        option_explicit: module.option_explicit,
        functions: functions,
        fields: fields,
        setters: setters,
        module_type: module_type,
      };
    
    module
  }
  
  fn load_module(file: &str)-> crate::runtime::Module {
    let module = crate::gen::parse(file);
    let base_name = file.split('/').last().unwrap().split('.').next().unwrap();
    let module_type =
      match file.split('.').last().unwrap() {
        "bas" => crate::runtime::ModuleType::Module,
        "cls" => crate::runtime::ModuleType::Class,
        _ => panic!("unknown file type"),
      };
        
    build_module(base_name, module_type, module)
  }
  
  pub fn load_modules(files: Vec<&String>, global_env: &mut Env) {
    for file in files {
      println!("input file: {}", file);
      
      let module = load_module(&file);
      global_env.append_global(hashmap![
        Rc::clone(&module.name) => Value::Definition(Definition::Module(Rc::new(module)))
      ]);
    }
  }
}

use std::env;
use std::rc::Rc;
  
fn main() {
  // 最初に読み込んだファイルのmain関数をエントリポイントとする。
  let args: Vec<String> = env::args().collect();
  let mut global_env = prepare::prepare_global_env();
  
  let args: Vec<&String> = args.iter().skip(1).collect();
  prepare::load_modules(args, &mut global_env);
  
  crate::static_checker::check(&global_env);
  
  let t = "test1".to_owned();
  let prog = crate::runtime::Program::new();
  let (result, env) = prog.evaluate_program(&mut global_env, Rc::new(t), Rc::new("main".to_owned()));
  
  println!("result: {:?}", result);
  
  (*env.get_system().worksheet).borrow().print_worksheet();
}
