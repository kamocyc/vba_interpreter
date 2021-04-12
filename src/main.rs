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

mod runtime {
  use crate::gen::ast::*;
  use std::collections::HashMap;
  
  #[derive(Debug, Clone)]
  pub struct Object {
    fields: HashMap<Id, Value>,
    methods: HashMap<Id, Function>,
  }
  
  #[derive(Debug, Clone)]
  pub enum Value {
    Int(i32),
    Bool(bool),
    String(String),
    Function(Function),
    Object(Object),
  }
  
  #[derive(Debug, Clone)]
  pub struct Env {
    global: HashMap<Id, Value>,
    stack: Vec<HashMap<Id, Value>>
  }
  
  macro_rules! hashmap {
    ($( $key: expr => $val: expr ),*) => {{
      let mut map = ::std::collections::HashMap::new();
      $( map.insert($key, $val); )*
      map
    }}
  }
  
  impl Env {
    pub fn new()-> Self {
      Self {
        global: HashMap::new(),
        stack: vec![]
      }
    }
    
    fn append_global(&mut self, values: HashMap<Id, Value>) {
      for value in values {
        self.global.insert(value.0, value.1);
      }
    }
    
    fn push_stack(&mut self) {
      self.stack.push(HashMap::new());
    }
    
    fn assign_local_var(&mut self, id: &String, value: Value) {
      self.stack.last_mut().unwrap().insert(id.clone(), value);
      println!("assigned: {:?}", self.stack);
    }
    
    fn get_opt(&self, id: &String)-> Option<&Value> {
      println!("get: {:?}", self.stack);
      match self.stack.last() {
        Some(v) => match v.get(id) {
          Some(v) => Some(v),
          None => match self.global.get(id) {
            Some(v) => Some(v),
            None => None
          }
        },
        None => match self.global.get(id) {
          Some(v) => Some(v),
          None => None
        }
      }
    }
    
    fn get(&self, id: &String)-> &Value {
      match self.get_opt(id) {
        Some (value) => value,
        None => panic!("name not found: {}", id)
      }
    }
  }
  
  pub struct Program {
  }
  
  impl Program {
    pub fn new()-> Self {
      Self {}
    }
  }
  
  macro_rules! assert_type {
    ($exp:expr, $pat:path) => {
      match $exp {
        $pat(v) => v,
        _ => {
          panic!("illelga type: {:?}", $exp)
        }
      }
    };
  }
  
  fn cast_to_int(value: Value)-> i32 {
    match value {
      Value::Int(i) => i,
      Value::Bool(b) => if b { 1 } else { 0 },
      Value::String(s) => {
        match s.parse::<i32>() {
          Ok(i) => i,
          _ => panic!("cannot convert to integer from string: {:?}", s)
        }
      },
      Value::Object(_) | Value::Function(_) => panic!("cast_to_int: illegal")
    }
  }
  
  fn cast_to_bool(value: Value)-> bool {
    match value {
      Value::Int(i) => i != 0,
      Value::Bool(b) => b,
      Value::String(s) => /* TODO */ s != "",
      Value::Object(_) | Value::Function(_) => panic!("cast_to_bool: illegal")
    }
  }
  
  fn cast_to_string(value: Value)-> String {
    match value {
      Value::Int(i) => i.to_string(),
      Value::Bool(b) => if b { "True".to_owned() } else { "False".to_owned() },
      Value::String(s) => s,
      Value::Object(_) | Value::Function(_) => panic!("cast_to_string: illegal")
    }
  }
  
  impl Program {
    pub fn evaluate_program(&self, module: Module, env: &mut Env, entry_function: &String)-> Value {
      let mut functions = HashMap::new();
      for function in module.functions {
        functions.insert(function.id.clone(), Value::Function(function));
      }
      
      functions.insert(
        "Debug".to_owned(),
        Value::Object(Object {
          fields: HashMap::new(),
          methods: hashmap!["Print".to_owned() => Function{
              id: "Print".to_owned(),
              parameters: vec!["message".to_owned()],
              body: FunctionBody::Native(crate::native::debug::print)
            }]
        }),
      );
      
      env.append_global(functions);
      
      println!("{:?}", env);
      
      match env.get_opt(entry_function) {
        Some(value) => {
          match value {
            Value::Function(function) => {
              // TODO: do not clone
              let function = &function.clone();
              self.invoke_function(env, function, &vec![])
            },
            _ => value.clone()
          }
        },
        None => panic!("entry_funciton {:?} not found", entry_function)
      }
    }
    
    fn invoke_function(&self, env: &mut Env, function: &Function, arguments: &Vec<Value>)-> Value {
      // println!("arguments={:?}, function={:?}", arguments, function);
      assert!(arguments.len() == function.parameters.len());
      
      match &function.body {
        FunctionBody::Native(function) => {
          function(arguments)
        },
        FunctionBody::VBA(body) => {
          env.push_stack();
          for (param, arg) in function.parameters.iter().zip(arguments) {
            env.assign_local_var(param, arg.clone());
          }
          
          env.assign_local_var(&function.id, Value::Int(0));
          
          self.evaluate_block(env, &(body));
          
          env.get(&function.id).clone()
        }
      }
      
    }
    
    fn evaluate_block(&self, env: &mut Env, expr: &Block) {
      for statement in &expr.statements {
        match statement {
          Statement::Expr(expr) => {
            // return value is disposed
            self.evaluate_expr(env, &expr);
          },
          Statement::If(expr, block1, block2) => {
            let val = cast_to_bool(self.evaluate_expr(env, expr));
            if val {
              self.evaluate_block(env, block1);
            } else {
              match block2 {
                Some(block2) => {
                  self.evaluate_block(env, block2);
                },
                None => {}
              }
            }
          },
          Statement::For(id, start_expr, end_expr, block) => {
            let val = self.evaluate_expr(env, start_expr);
            env.assign_local_var(id, val);
            loop {
              let val = self.evaluate_expr(env, end_expr);
              if *assert_type!(env.get(id), Value::Int) > cast_to_int(val) {
                break;
              }
              
              self.evaluate_block(env, block);
              
              let t = assert_type!(env.get(id), Value::Int) + 1;
              env.assign_local_var(id, Value::Int(t));
            }
          },
          Statement::Assign(id, expr) => {
            match id {
              Chain::App(App {id, arguments}) => {
                if arguments.len() == 0 {
                  let val = self.evaluate_expr(env, expr);
                  env.assign_local_var(id, val);
                } else {
                  panic!();
                }
              },
              _ => panic!()
            }
          },
        }
      }
    }

    fn evaluate_app(&self, env: &mut Env, app: &App)-> Value {
      let App {id, arguments} = app;
      
      match env.get_opt(id) {
        Some(value) => {
          match value {
            Value::Function(function) => {
              // TODO: do not clone
              let function = &function.clone();
              let mut args = vec![];
              for arg in arguments {
                args.push(self.evaluate_expr(env, arg));
              }
              
              self.invoke_function(env, function, &args)
            },
            _ => {
              assert!(arguments.len() == 0);
              value.clone()
            }
          }
        },
        None => panic!("not found: {:?}", id)
      }
    }
    
    fn invoke_method(&self, env: &mut Env, object: &Object, app: &App)-> Value {
      let App {id, arguments} = app;
      
      match object.methods.get(id) {
        Some(function) => {
          let args: Vec<Value> = arguments.iter().map(|arg| self.evaluate_expr(env, arg)).collect();
          self.invoke_function(env, function, &args)
        },
        None => {
          match object.fields.get(id) {
            Some(value) => {
              assert!(arguments.len() == 0);
              value.clone()
            },
            None => panic!("invoke method, not found: {:?}", id)
          }
        }
      }
    }
    
    fn evaluate_chain(&self, env: &mut Env, chain: &Chain)-> Value {
      match chain {
        Chain::App(app) => {
          self.evaluate_app(env, app)
        },
        Chain::Method(receiver, message) => {
          let value = self.evaluate_chain(env, receiver);
          let value = assert_type!(value, Value::Object);
          self.invoke_method(env, &value, message)
        }
      } 
    }
    
    fn evaluate_expr(&self, env: &mut Env, expr: &Expr)-> Value {
      match expr {
        Expr::BinOp(op, e1, e2) => {
          match op {
            Op::Add | Op::Sub | Op::Mul | Op::Div => {
              let op = match op {
                Op::Add => |x, y| x + y,
                Op::Sub => |x, y| x - y,
                Op::Mul => |x, y| x * y,
                Op::Div => |x, y| x / y,
                _ => panic!()
              };
              
              Value::Int(
                op(
                  cast_to_int(self.evaluate_expr(env, e1)),
                  cast_to_int(self.evaluate_expr(env, e2))))
            },
            Op::Geq | Op::Gt | Op::Leq | Op::Lt | Op::Equal | Op::Neq => {
              let op = match op {
                Op::Geq => |x, y| x >= y,
                Op::Gt  => |x, y| x >  y,
                Op::Leq => |x, y| x <= y,
                Op::Lt  => |x, y| x <  y,
                Op::Equal => |x, y| x == y,
                Op::Neq => |x, y| x != y,
                _ => panic!()
              };
              
              Value::Bool(
                op(
                  cast_to_int(self.evaluate_expr(env, e1)),
                  cast_to_int(self.evaluate_expr(env, e2))))
            },
            Op::And | Op::Or => {
              let op = match op {
                Op::And => |x, y| x && y,
                Op::Or => |x, y| x || y,
                _ => panic!()
              };
              
              Value::Bool(
                op(
                  cast_to_bool(self.evaluate_expr(env, e1)),
                  cast_to_bool(self.evaluate_expr(env, e2))))
            },
            Op::Concat => {
              let s1 = cast_to_string(self.evaluate_expr(env, e1));
              let s2 = cast_to_string(self.evaluate_expr(env, e2));
              let s = s1 + &s2;
              Value::String(s.clone())
            },
          }
        },
        Expr::Int(i) => {
          Value::Int(*i)
        },
        Expr::String(s) => {
          Value::String(s.clone())
        },
        Expr::Var(chain) => {
          self.evaluate_chain(env, chain)
        },
      }
    }
  }
}

fn main() {
  let filename = "test/test1.bas";
  let module = crate::gen::parse(filename);
  
  let mut global_env = crate::runtime::Env::new();
  let prog = crate::runtime::Program::new();
  let result = prog.evaluate_program(module, &mut global_env, &"main".to_owned());
  
  println!("result: {:?}", result);
}
