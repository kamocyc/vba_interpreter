use std::rc::Rc;
use std::cell::RefCell;
use crate::gen::ast::*;
use std::collections::HashMap;

pub type Id = Rc<String>;

#[derive(Debug)]
pub struct Object {
  pub fields: RefCell<HashMap<Id, Value>>,
  pub methods: HashMap<Id, Rc<Function>>,
  pub setters: HashMap<Id, Rc<Function>>,
  pub default_property: Option<Id>,
  pub source_module: Option<Id>,
}

#[derive(Debug)]
pub struct Parameter {
  pub name: Id,
  pub typename: Typename,
  pub is_optional: bool,
  pub is_param_array: bool,
}

pub fn param(name: &str, typename: Typename)-> Parameter {
  Parameter {
    name: Rc::new(name.to_string()),
    typename,
    is_optional: false,
    is_param_array: false,
  }
}

#[derive(Debug)]
pub struct Function {
  pub modifier: Modifier,
  pub function_type: FunctionType,
  pub name: Id,
  pub parameters: Vec<Parameter>,
  pub body: FunctionBody,
  pub return_typename: Typename
}

impl Function {
  pub fn new(ast_function: crate::gen::ast::Function)-> Self {
    Self {
      modifier: ast_function.modifier,
      function_type: ast_function.function_type,
      name: ast_function.name,
      parameters: ast_function.parameters.iter().map(|param| Parameter {name: Rc::clone(&param.name), typename: param.typename.clone(), is_optional: false, is_param_array: false}).collect(),
      body: FunctionBody::VBA(ast_function.body),
      return_typename: ast_function.return_typename,
    }
  }
}

pub enum FunctionBody {
  Native(Box<fn(&Env, &Vec<crate::runtime::Value>) -> crate::runtime::Value>),
  VBA(Block)
}
impl std::fmt::Debug for FunctionBody {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    f.debug_tuple("").finish()
  }
}

#[derive(Debug, Clone)]
pub enum Value {
  Integer(i32),
  Bool(bool),
  String(Rc<String>),
  Definition(Definition),
  Object(Rc<Object>),
  Array(Rc<RefCell<Array>>),
  Null,
  Nothing,
}

#[derive(Debug)]
pub struct Array {
  pub data: Vec<Value>,
}

impl Array {
  pub fn new(data: Vec<Value>) -> Self {
    Self {
      data
    }
  }
}

#[derive(Debug, Clone)]
pub enum Definition {
  Module(Rc<Module>),
  Function(Rc<Function>),
  Setter(Rc<Function>, Rc<Object>),
}

#[derive(Debug)]
pub struct Module {
  pub name: Id,
  pub option_explicit: bool,
  pub fields: HashMap<Id, Typename>,
  pub functions: HashMap<Id, Rc<Function>>,
  pub setters: HashMap<Id, Rc<Function>>,
  pub module_type: ModuleType,
}

#[derive(Debug)]
pub enum ModuleType {
  Module,
  Class,
}

impl Module {
  pub fn get(&self, id: &String)-> Option<Rc<Function>> {
    self.functions.get(id).map(|x| Rc::clone(x))
  }
}

pub fn get_default_value(typename: &Typename)-> Value {
  match typename {
    Typename::Integer => Value::Integer(0),
    Typename::Boolean => Value::Bool(false),
    Typename::String => Value::String(Rc::new("".to_string())),
    Typename::Variant => Value::Null,
    Typename::Object => Value::Nothing,
    Typename::Id(_) => Value::Nothing, // TODO: 値型の対応
  }
}

#[derive(Debug,Clone)]
pub struct System {
  pub worksheet: Rc<RefCell<Worksheet>>
}

macro_rules! clone_rc_opt {
  ($x:expr) => {
    $x.as_ref().map(|x| Rc::clone(x))
  }
}

static WORKSHEET_WIDTH: usize = 100;
static WORKSHEET_HEIGHT: usize = 100;

#[derive(Debug,Clone)]
pub struct Worksheet {
  cells: Vec<Vec<Value>>,
}

// use cli_table::{print_stdout, Cell, Table};

// ?
fn to_alpha(column_index: usize)-> String {
  let mut result = String::new();
  let mut column_index = column_index;
  while column_index > 0 {
    let remainder = (column_index - 1) % 26;
    result.push(('A' as u8 + remainder as u8) as char);
    column_index = (column_index - 1) / 26;
  }
  result.chars().rev().collect()
}

impl Worksheet {
  pub fn new()-> Self {
    let mut cells = vec![];
    for _i in 0..WORKSHEET_HEIGHT {
      let mut row = vec![];
      for _j in 0..WORKSHEET_WIDTH {
        row.push(Value::Integer(0));
      }
      cells.push(row);
    }
    
    for row in 1..WORKSHEET_HEIGHT {
      cells[row][0] = Value::Integer(row as i32);
    }
    for col in 1..WORKSHEET_WIDTH {
      cells[0][col] = Value::String(Rc::new(to_alpha(col)));
    }
    
    Self {
      cells: cells
    }
  }
  
  pub fn get_range(ws: Rc<RefCell<Self>>, row: i32, col: i32)-> CellRange {
    CellRange {
      worksheet: ws,
      range: Range {
        start_row: row,
        start_col: col,
        end_row: row,
        end_col: col,
      }
    }
  }
  
  pub fn print_worksheet(&self) {
    let mut last_row = 0;
    let mut last_col = 0;
    for row in 1..WORKSHEET_HEIGHT {
      for col in 1..WORKSHEET_WIDTH {
        match self.cells[row][col] {
          Value::Integer(i) => if i != 0 {
            last_row = row;
            if col > last_col { last_col = col; }
          },
          _ => {
            last_row = row;
            if col > last_col { last_col = col; }
          }
        }
      }
    }
    
    // let table: Vec<Vec<_>> =
    //   self.cells[0..last_row+1].iter().map(|row| {
    //     row[0..last_col+1].iter().map(|cell| {
    //       cast_to_string(cell.clone()).cell()
    //     }).collect()
    //   }).collect();
    
    // print_stdout(table.table()).unwrap();
  }
}

pub struct Range {
  pub start_row: i32,
  pub start_col: i32,
  pub end_row: i32,
  pub end_col: i32,
}

impl Range {
  // pub fn new(start_row: i32, start_col: i32, end_row: i32, end_col: i32)-> Self {
  //   Self {
  //     start_row: start_row,
  //     start_col: start_col,
  //     end_row: end_row,
  //     end_col: end_col,
  //   }
  // }
  
  pub fn new_singleton(row: i32, col: i32)-> Self {
    Self {
      start_row: row,
      start_col: col,
      end_row: row,
      end_col: col,
    }
  }
}

pub struct CellRange {
  worksheet: Rc<RefCell<Worksheet>>,
  range: Range,
}

impl CellRange {
  pub fn get_value(&self)-> Value {
    let row = self.range.start_row as usize;
    let col = self.range.start_col as usize;
    (*self.worksheet).borrow().cells[row][col].clone()
  }
  
  pub fn set_value(&mut self, value: Value) {
    let row = self.range.start_row as usize;
    let col = self.range.start_col as usize;
    (*self.worksheet).borrow_mut().cells[row][col] = value;
  }
}

#[derive(Debug)]
pub struct Env {
  global: HashMap<Id, Value>,
  stack: Vec<HashMap<Id, Value>>,
  system: System,
  // object_fields: HashMap<Id, Value>,
  object: Option<Rc<Object>>,
}

// use std::rc::Rc;

impl Env {
  
  pub fn new(system: System, global: Vec<(Id, Value)>)-> Self {
    let mut g = HashMap::new();
    for (id, g_) in global {
      g.insert(Rc::clone(&id), g_);
    }
    
    Self {
      global: g,
      stack: vec![],
      system: system,
      // object_fields: vec![],
      object: None,
    }
  }
  
  pub fn get_system(&self)-> &System {
    &self.system
  }
  
  pub fn get_global_names(&self)-> Vec<Id> {
    self.global.keys().map(|id| Rc::clone(id)).collect()
  }
  
  pub fn append_global(&mut self, values: HashMap<Id, Value>) {
    for value in values {
      self.global.insert(value.0, value.1);
    }
  }
  
  fn set_object_env(&mut self, object: Option<Rc<Object>>) {
    match object {
      Some(object) => {
        self.object = Some(Rc::clone(&object));
      },
      None => {
        self.object = None;
      },
    }
  }
  
  fn push_stack(&mut self) {
    self.stack.push(HashMap::new());
  }
  
  fn pop_stack(&mut self)-> HashMap<Id, Value> {
    self.stack.pop().unwrap()
  }
  
  fn assign_local_var(&mut self, id: Rc<String>, value: Value) {
    self.stack.last_mut().unwrap().insert(id, value);
    // println!("assigned: {:?}", self.stack);
  }
  
  fn assign_var(&mut self, current_module: Option<Rc<String>>, id: Rc<String>, value: Value) {
    fn assign_var_module(_env: &mut Env, _current_module: Option<Rc<String>>, _id: Rc<String>, _value: Value) {
      // TODO: 
      panic!("not implemented");
    }
    
    fn assign_var_object(s: &mut Env, current_module: Option<Rc<String>>, id: Rc<String>, value: Value) {
      // 2. instance variable
      let flag;
      match &s.object {
        Some(o) => {
          match o.fields.borrow().get(&id) {
            Some(_) => {
              flag = 2;
            },
            None => {
              flag = 1;
            },
          }
          
          if flag == 2 {
            o.fields.borrow_mut().insert(id, value);
            return;
          }
        },
        None => {
          assign_var_module(s, current_module, id, value);
          return;
        },
      }
      
      if flag == 1 {
        assign_var_module(s, current_module, id, value);
      }
    }
    
    // 1. local variable
    match self.stack.last_mut() {
      Some(stack) =>
        match stack.get(&id) {
          Some(_) => {
            self.assign_local_var(id, value);
          },
          None => {
            assign_var_object(self, current_module, id, value);
          }
        },
      None => assign_var_object(self, current_module, id, value),
    }
  }
  
  pub fn get_field_value_opt(&self, id: &String)-> Option<Value> {
    match &self.object {
      Some(object) => {
        object.fields.borrow().get(id).map(|value| value.clone())
      },
      None => None,
    }
  }
  
  fn get_opt(&self, curret_module: Option<Rc<String>>, id: &String)-> Option<Value> {
    // println!("get: {:?}", self.stack);
    fn get_opt_from_global(s: &Env, id: &String)-> Option<Value> {
      match s.global.get(id) {
        Some(v) => Some(v.clone()),
        None => None
      }
    }
    
    fn get_opt_from_module(s: &Env, curret_module: Option<Rc<String>>, id: &String)-> Option<Value> {
      match curret_module {
        Some(curret_module) =>
          match s.get_opt(None, &(*curret_module)) {
            Some(Value::Definition(Definition::Module(module))) => {
              match module.get(id) {
                Some(v) => Some(Value::Definition(Definition::Function(v))),
                None => get_opt_from_global(s, id),
              }
            },
            _ => get_opt_from_global(s, id),
          }
        None => get_opt_from_global(s, id),
      }
    }
    
    fn get_opt_from_object(s: &Env, curret_module: Option<Rc<String>>, id: &String)-> Option<Value> {
      match &s.object {
        Some(object) => {
          match object.fields.borrow().get(id) {
            Some(v) => Some(v.clone()),
            None => get_opt_from_module(s, curret_module, id)
          }
        },
        None => get_opt_from_module(s, curret_module, id)
      }
    }
    
    match self.stack.last() {
      Some(v) => match v.get(id) {
        Some(v) => Some(v.clone()),
        None => get_opt_from_object(self, curret_module, id)
      },
      None => get_opt_from_object(self, curret_module, id)
    }
  }
  
  fn get_from_stack(&self, id: &String)-> Option<Value> {
    match self.stack.last() {
      Some(v) => match v.get(id) {
        Some(v) => Some(v.clone()),
        None => None
      },
      None => None
    }
  }
  
  fn get(&self, curret_module: Option<Rc<String>>, id: &String)-> Value {
    match self.get_opt(curret_module, id) {
      Some (value) => value,
      None => panic!("name not found: {}", id)
    }
  }
  
  fn is_from_object(&self, id: &String)-> bool {
    match &self.object {
      None => false,
      Some(object) => {
        object.fields.borrow().contains_key(id) && !self.stack.iter().any(|v| v.contains_key(id))
      }
    }
  }
  
  pub fn get_vba_modules(&self)-> Vec<Rc<Module>> {
    let mut modules = Vec::new();
    for (_, value) in self.global.iter() {
      match value {
        Value::Definition(Definition::Module(module)) => {
          modules.push(Rc::clone(module));
        },
        _ => (),
      }
    }
    modules
  }
}

pub struct Program {
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

pub fn cast_to_int(value: Value)-> i32 {
  match value {
    Value::Integer(i) => i,
    Value::Bool(b) => if b { 1 } else { 0 },
    Value::String(s) => {
      match s.parse::<i32>() {
        Ok(i) => i,
        _ => panic!("cannot convert to integer from string: {:?}", s)
      }
    },
    Value::Null | Value::Nothing => 0,
    Value::Array(_) | Value::Object(_) | Value::Definition(_) => panic!("cast_to_int: illegal")
  }
}

fn cast_to_bool(value: Value)-> bool {
  match value {
    Value::Integer(i) => i != 0,
    Value::Bool(b) => b,
    Value::String(s) =>
      match s.to_lowercase().as_str() {
        "true" => true,
        "false" => false,
        _ => panic!("cannot convert to boolean from string: {:?}", s)
      },
    Value::Null | Value::Nothing => false,
    Value::Array(_) | Value::Object(_) | Value::Definition(_) => panic!("cast_to_bool: illegal")
  }
}

pub fn cast_to_string(value: Value)-> Rc<String> {
  match value {
    Value::Integer(i) => Rc::new(i.to_string()),
    Value::Bool(b) => if b { Rc::new("True".to_owned()) } else { Rc::new("False".to_owned()) },
    Value::String(s) => s,
    Value::Null => Rc::new("Null".to_owned()),
    Value::Nothing => Rc::new("Nothing".to_owned()),
    Value::Array(array) => {
      let mut s = "[".to_owned();
      for (i, v) in array.borrow().data.iter().enumerate() {
        if i != 0 {
          s.push_str(", ");
        }
        s.push_str(&cast_to_string(v.clone()).to_string());
      }
      s.push_str("]");
      Rc::new(s)
    },
    Value::Object(_) | Value::Definition(_)  => panic!("cast_to_string: illegal")
  }
}

impl Program {
  pub fn new()-> Self { Self {} }
  
  pub fn evaluate_program<'a>(&self, env: &'a mut Env, entry_module: Rc<String>, entry_function: Rc<String>)-> (Value, &'a Env) {
    // println!("{:?}", env);
    
    match env.get_opt(Some(Rc::clone(&entry_module)), &entry_function) {
      Some(value) => {
        match value {
          Value::Definition(Definition::Function(function)) => {
            let v = self.invoke_function(env, Some(Rc::clone(&entry_module)), None, function, &vec![]);
            (v, env)
          },
          _ => (value.clone(), env)
        }
      },
      None => panic!("entry_funciton {:?} not found", entry_function)
    }
  }
  

  fn invoke_default_property(&self, env: &mut Env, object: Rc<Object>, arguments: &Vec<Value>)-> Value {
    match &object.default_property {
      Some(v) => {
        self.invoke_method(env, Rc::clone(&object), &v, arguments, false)
      },
      None => panic!("no default property: {:?}", object)
    }
  }
  
  fn to_primitive_value(&self, env: &mut Env, value: Value)-> Value {
    match value {
      Value::Object(object) => {
        self.invoke_default_property(env, Rc::clone(&object), &vec![])
      },
      _ => value
    }
  }

  fn invoke_function(&self, env: &mut Env, current_module: Option<Rc<String>>, object: Option<Rc<Object>>, function: Rc<Function>, arguments: &Vec<Value>)-> Value {
    // println!("arguments={:?}, function={:?}", arguments, function);
    let parameter_typenames: Vec<Typename> = {
      if arguments.len() <= function.parameters.len() {
        let required_arguments_number = function.parameters.iter().filter(|p| !p.is_optional).count();
        if arguments.len() < required_arguments_number {
          panic!("too few arguments (expected: {}, actual: {}) (function: {:?})", function.parameters.len(), arguments.len(), function);
        }
        
        function.parameters.iter().take(arguments.len()).map(|x| x.typename.clone()).collect()
      } else {
        let last_parameter = function.parameters.last().unwrap();
        if !last_parameter.is_param_array {
          panic!("too many arguments (expected: {}, actual: {}) (function: {:?})", function.parameters.len(), arguments.len(), function);
        }
        
        let mut parameter_typenames: Vec<Typename> = function.parameters.iter().map(|x| x.typename.clone()).collect();
        for _i in function.parameters.len()..arguments.len() {
          parameter_typenames.push(last_parameter.typename.clone());
        }
        
        parameter_typenames
      }
    };
    
    let parameters: Vec<(&Typename, Value)> =
      parameter_typenames.iter().zip(arguments.iter()).map(|(par, arg)|
        match (&par, arg) {
          | (Typename::Object, _) | (Typename::Id(_), _)=> {
            // TODO: 構造体型などはValueにする
            (par, arg.clone())
          },
          | (_, Value::Object(_)) => {
            let v = self.to_primitive_value(env, arg.clone());
            (par, v)
          },
          | _ => (par, arg.clone())
        }
      ).collect();
    
    let mut arguments = vec![];
    for (par, arg) in parameters.iter() {
      match (&par, arg) {
        | (Typename::Integer, Value::Integer(_))
        | (Typename::Boolean, Value::Bool(_))
        | (Typename::String, Value::String(_))
        | (Typename::Variant, _)
        | (Typename::Object, Value::Object(_))
          => {
            arguments.push(arg.clone());
          },
        | (Typename::Id(id), Value::Object(object)) => {
          match &object.source_module {
            Some(source_module) => {
              if source_module == id {
                arguments.push(arg.clone());
              } else{
                panic!("argument type mismatch (2) (expected: {:?}, actual: {:?})", par, arg)
              }
            },
            None => panic!("argument type mismatch (2) (expected: {:?}, actual: {:?})", par, arg)
          }
        },
        | (Typename::Integer, _) => {
          arguments.push(Value::Integer(cast_to_int(arg.clone())));
        }
        | (Typename::Boolean, _) => {
          arguments.push(Value::Bool(cast_to_bool(arg.clone())));
        }
        | (Typename::String, _) => {
          arguments.push(Value::String(cast_to_string(arg.clone())));
        }
        _ => panic!("argument type mismatch (expected: {:?}, actual: {:?})", par, arg)
      }
    }
    
    let arguments: Vec<_> = parameters.iter().map(|(_, arg)| arg.clone()).collect();
    
    env.set_object_env(object);
    
    match &function.body {
      FunctionBody::Native(function) => {
        function(env, &arguments)
      },
      FunctionBody::VBA(body) => {
        env.push_stack();
        for (param, arg) in function.parameters.iter().zip(arguments) {
          env.assign_local_var(Rc::clone(&param.name), arg.clone());
        }
        
        // return value of called function
        env.assign_local_var(Rc::clone(&function.name), Value::Integer(0));
        
        self.evaluate_block(env, current_module, &(body));
        
        let return_value = env.get(None, &function.name).clone();
        
        env.pop_stack();
        
        return_value
      }
    }
  }
  
  fn invoke_setter(&self, env: &mut Env, current_module: Option<Rc<String>>, id: &Chain, rhs: Value) {
    let lvalue_result = self.evaluate_chain(env, current_module, id, Some(rhs.clone()), true, false);
    match lvalue_result {
      Ok(lvalue) => {
        match lvalue {
          Value::Definition(Definition::Setter(function, object)) => {
            // println!("setter: {:?}", function);
            self.invoke_function(env, clone_rc_opt!(object.source_module), Some(Rc::clone(&object)), function, &vec![rhs]);
          },
          Value::Object(object) => {
            match object.default_property {
              Some(ref default_property) => {
                match object.setters.get(default_property) {
                  Some(setter) => {
                    self.invoke_function(env, clone_rc_opt!(object.source_module), Some(Rc::clone(&object)), Rc::clone(setter), &vec![rhs]);
                  },
                  None => { panic!("illegal default property name ({})", default_property); }
                }
              },
              None => { panic!("no default property"); }
            }
          },
          _ => { panic!("illegal lvalue (1): {:?}", lvalue); }
        }
      },
      Err((receiver, field_name)) => {
        match receiver {
          Value::Object(object) => {
            let flag;
            match object.fields.borrow().get(&field_name) {
              Some(_) => { flag = true; }
              None => { flag = false; }
            }
            if flag {
              let mut fields = object.fields.borrow_mut();
              fields.insert(Rc::clone(&field_name), rhs);
            } else {
              panic!("illegal field name ({})", field_name);
            }
          },
          Value::Array(_) => (),
          _ => panic!("illegal lvalue (2): {:?}", receiver)
        }
      }
    }
  }
  
  fn evaluate_block(&self, env: &mut Env, current_module: Option<Rc<String>>, expr: &Block) {
    for statement in &expr.statements {
      match statement {
        Statement::Expr(expr) => {
          // return value is discarded
          self.evaluate_expr(env, clone_rc_opt!(current_module), &expr);
        },
        Statement::If(expr, block1, block2) => {
          let val = cast_to_bool(self.evaluate_expr_primitive(env, clone_rc_opt!(current_module), expr));
          if val {
            self.evaluate_block(env, clone_rc_opt!(current_module), block1);
          } else {
            match block2 {
              Some(block2) => {
                self.evaluate_block(env, clone_rc_opt!(current_module), block2);
              },
              None => {}
            }
          }
        },
        Statement::For(id, start_expr, end_expr, block) => {
          let val = self.evaluate_expr_primitive(env, clone_rc_opt!(current_module), start_expr);
          env.assign_var(clone_rc_opt!(current_module), Rc::clone(id), val);
          loop {
            let val = self.evaluate_expr_primitive(env, clone_rc_opt!(current_module), end_expr);
            if assert_type!(env.get(clone_rc_opt!(current_module), id), Value::Integer) > cast_to_int(val) {
              break;
            }
            
            self.evaluate_block(env, clone_rc_opt!(current_module), block);
            
            let t = assert_type!(env.get(clone_rc_opt!(current_module), id), Value::Integer) + 1;
            env.assign_var(clone_rc_opt!(current_module), Rc::clone(id), Value::Integer(t));
          }
        },
        Statement::DoWhile(expr, block) => {
          loop {
            let val = self.evaluate_expr_primitive(env, clone_rc_opt!(current_module), expr);
            if !cast_to_bool(val) {
              break;
            }
            
            self.evaluate_block(env, clone_rc_opt!(current_module), block);
          }
        },
        Statement::Assign(assign_mode, id, expr) => {
          let rhs =
            match assign_mode {
              AssignMode::Let => {
                self.evaluate_expr_primitive(env, clone_rc_opt!(current_module), expr)
              },
              AssignMode::Set => {
                self.evaluate_expr(env, clone_rc_opt!(current_module), expr)
              },
            };
          
          match id {
            Chain::App(App {name, arguments}) => {
              if arguments.len() == 0 {
                env.assign_var(clone_rc_opt!(current_module), Rc::clone(name), rhs);
              } else {
                self.invoke_setter(env, clone_rc_opt!(current_module), id, rhs);
              }
            },
            _ => self.invoke_setter(env, clone_rc_opt!(current_module), id, rhs)
          }
        },
        Statement::VariableDeclaration(VariableDeclaration {name, typename, ..}) => {
          // initialize variable if it is not assigned already
          match env.get_from_stack(name) {
            None => {
              let default_value = get_default_value(typename);
              env.assign_local_var(Rc::clone(name), default_value);
            },
            Some(_) => {},
          }
        }
      }
    }
  }

  fn evaluate_app(&self, env: &mut Env, current_module: Option<Rc<String>>, app: &App, rhs: Option<Value>, _is_lhs: bool, has_proceeding_chain: bool)-> Result<Value, (Value, Id)> {
    let App {name, arguments} = app;
    
    match env.get_opt(clone_rc_opt!(current_module), name) {
      Some(value) => {
        match value {
          Value::Definition(Definition::Function(function)) => {
            let args: Vec<Value> = arguments.iter().map(|arg| self.evaluate_expr(env, clone_rc_opt!(current_module), arg)).collect();
            
            let object =
              match &env.object {
                None => None,
                Some(o) => Some(Rc::clone(o)),
              };
              
            if env.is_from_object(name) {
              let source_module =
                match object {
                  None => None,
                  Some(ref o) => clone_rc_opt!(o.source_module)
                };
              Ok(self.invoke_function(env, clone_rc_opt!(source_module), clone_rc_opt!(object), function, &args))
            } else {
              // TODO?
              Ok(self.invoke_function(env, clone_rc_opt!(current_module), None, function, &args))
            }
          },
          Value::Object(ref object) => {
            if !has_proceeding_chain {
              match object.default_property {
                Some(_) => {
                  let args: Vec<Value> = arguments.iter().map(|arg| self.evaluate_expr(env, clone_rc_opt!(current_module), arg)).collect();
                  Ok(self.invoke_default_property(env, Rc::clone(object), &args))
                },
                None => {
                  assert!(arguments.len() == 0);
                  Ok(value.clone())
                }
              }
            } else {
              assert!(arguments.len() == 0);
              Ok(value.clone())
            }
          },
          Value::Array(ref array) => {
            if arguments.len() == 0 {
              Ok(value.clone())
            } else if arguments.len() == 1 {
              let index = self.evaluate_expr_primitive(env, clone_rc_opt!(current_module), &arguments[0]);
              let index = cast_to_int(index);
              if index < 0 || index >= array.borrow().data.len() as i32 {
                panic!("index out of range");
              }
              match rhs {
                None => Ok(array.borrow().data[index as usize].clone()),
                Some(rhs) => {
                  // println!("rhs: {:?}", rhs);
                  array.borrow_mut().data[index as usize] = rhs;
                  // println!("array: {:?}", array);
                  Err((value, Rc::new("".to_string())))
                }
              }
            } else {
              panic!("illegal number of arguments");
            }
          },
          _ => {
            // variable
            assert!(arguments.len() == 0);
            Ok(value.clone())
          }
        }
      },
      None => {
        env.assign_local_var(Rc::clone(&name), Value::Integer(0));
        Ok(Value::Integer(0))
      }
    }
  }
  
  fn invoke_method(&self, env: &mut Env, object: Rc<Object>, name: &String, args: &Vec<Value>, is_lhs: bool)-> Value {
    if is_lhs {
      match object.setters.get(name) {
        Some(setter) => {
          return Value::Definition(Definition::Setter(setter.clone(), Rc::clone(&object)))
        },
        None => ()
      }
    }
    
    match object.methods.get(name) {
      Some(function) => {
        self.invoke_function(env, clone_rc_opt!(object.source_module), Some(Rc::clone(&object)), Rc::clone(function), &args)
      },
      None => {
        if is_lhs {
          panic!("not implemented");
          // match object.fields.get(name) {
          //   Some(value) => {
          //     let name1 = Rc::new(name.clone());
          //     Value::Definition(Definition::Function(Rc::new(Function {
          //       id: Rc::new(name.clone()),
          //       parameters: vec![Parameter {
          //         name: Rc::new("value".to_string()),
          //         typename: Typename::Variant,
          //       }],
          //       body: FunctionBody::Native(Box::new(|env, argments| {
          //         env.assign_local_var(name1, argments[0].clone());
          //         Value::Integer(0)
          //       })),
          //     })))
          //   },
          //   None => panic!("invoke method, not found (2): {:?}", name)
          // }
        } else {
          match object.fields.borrow().get(name) {
            Some(value) => {
              assert!(args.len() == 0);
              value.clone()
            },
            None => panic!("invoke method, not found: {:?}", name)
          }
        }
      }
    }
  }
  
  fn evaluate_chain(&self, env: &mut Env, current_module: Option<Rc<String>>, chain: &Chain, rhs: Option<Value>, is_lhs: bool, has_proceeding_chain: bool)-> Result<Value, (Value, Id)> {
    match chain {
      Chain::App(app) => {
        self.evaluate_app(env, current_module, app, rhs, is_lhs, has_proceeding_chain)
      },
      Chain::Method(receiver, message) => {
        let object = self.evaluate_chain(env, clone_rc_opt!(current_module), receiver, None, false, true)?;
        match &object {
          Value::Object(ref value) => {
            // propertyのとき
            if message.arguments.len() == 0 && is_lhs {
              match value.fields.borrow().get(&message.name) {
                Some (_) => {
                  // panic!("chain: {:?}, receiver: {:?}, message: {:?}, value: {:?}", chain, receiver, message, value);
                  return Err((object.clone(), Rc::clone(&message.name)));
                },
                None => ()
              }
            }
            
            let args: Vec<Value> = message.arguments.iter().map(|arg| self.evaluate_expr(env, clone_rc_opt!(current_module), arg)).collect();
            Ok(self.invoke_method(env, Rc::clone(value), &message.name, &args, is_lhs))
          },
          Value::Definition(Definition::Module(module)) => {
            assert!(message.arguments.len() == 0);
            Ok(Value::Definition(Definition::Function(module.get(&message.name).unwrap())))
          }
          , _ => panic!()
        }
      }
    } 
  }
  
  fn evaluate_expr_primitive(&self, env: &mut Env, current_module: Option<Rc<String>>, e: &Expr)-> Value {
    let e = self.evaluate_expr(env, current_module, e);
    let e = self.to_primitive_value(env, e);
    e
  }
  
  fn evaluate_expr(&self, env: &mut Env, current_module: Option<Rc<String>>, expr: &Expr)-> Value {
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
            
            Value::Integer(
              op(
                cast_to_int(self.evaluate_expr_primitive(env, clone_rc_opt!(current_module), e1)),
                cast_to_int(self.evaluate_expr_primitive(env, clone_rc_opt!(current_module), e2))))
          },
          Op::Geq | Op::Gt | Op::Leq | Op::Lt | Op::Equal | Op::Neq => {
            let op = match op {
              Op::Geq   => |x, y| x >= y,
              Op::Gt    => |x, y| x >  y,
              Op::Leq   => |x, y| x <= y,
              Op::Lt    => |x, y| x <  y,
              Op::Equal => |x, y| x == y,
              Op::Neq   => |x, y| x != y,
              _ => panic!()
            };
            
            Value::Bool(
              op(
                cast_to_int(self.evaluate_expr_primitive(env, clone_rc_opt!(current_module), e1)),
                cast_to_int(self.evaluate_expr_primitive(env, clone_rc_opt!(current_module), e2))))
          },
          Op::And | Op::Or => {
            let op = match op {
              Op::And => |x, y| x && y,
              Op::Or => |x, y| x || y,
              _ => panic!()
            };
            
            Value::Bool(
              op(
                cast_to_bool(self.evaluate_expr_primitive(env, clone_rc_opt!(current_module), e1)),
                cast_to_bool(self.evaluate_expr_primitive(env, clone_rc_opt!(current_module), e2))))
          },
          Op::Concat => {
            let s1 = cast_to_string(self.evaluate_expr_primitive(env, clone_rc_opt!(current_module), e1));
            let s2 = cast_to_string(self.evaluate_expr_primitive(env, clone_rc_opt!(current_module), e2));
            let s = (*s1).clone() + &s2;
            Value::String(Rc::new(s))
          },
        }
      },
      Expr::Integer(i) => {
        Value::Integer(*i)
      },
      Expr::String(s) => {
        Value::String(Rc::clone(s))
      },
      Expr::Var(chain) => {
        self.evaluate_chain(env, clone_rc_opt!(current_module), chain, None, false, false).ok().unwrap()
      },
      Expr::New(id) => {
        match env.get_opt(None, id) {
          Some(value) => {
            match value {
              Value::Definition(Definition::Module(class)) => {
                match class.module_type {
                  ModuleType::Class => {
                    let mut fields = HashMap::new();
                    for (name, typename) in class.fields.iter() {
                      fields.insert(name.clone(), get_default_value(typename));
                    }
                    
                    let object = Object {
                      source_module: Some(Rc::clone(&class.name)),
                      default_property: None,
                      fields: RefCell::new(fields),
                      methods: class.functions.clone(),
                      setters: class.setters.clone(),
                    };
                  
                    Value::Object(Rc::new(object))
                  },
                  _ => panic!()
                }
              },
              _ => panic!()
            }
          },
          None => panic!("new: not found: {:?}", id),
        }
      }
    }
  }
}