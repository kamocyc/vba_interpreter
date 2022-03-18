pub mod ast;
  
mod vbalexer;
mod vbavisitor;
mod vbalistener;
mod vbaparser;

use vbavisitor::vbaVisitor;

use antlr_rust::tree::{ ParseTreeVisitor, Visitable, TerminalNode };
use crate::gen::vbaparser::*;

use antlr_rust::InputStream;
use antlr_rust::common_token_stream::CommonTokenStream;
use antlr_rust::token::Token;
use antlr_rust::tree::VisitChildren;

use std::rc::Rc;
use std::borrow::Cow;
use std::collections::VecDeque;

use crate::gen::ast::*;

#[derive(Debug, Clone)]
enum ASTNode {
  Expr(Expr),
  Chain(Chain),
  App(App),
  Function(Function),
  Block(Block),
  Param(Parameter),
  Params(Vec<Parameter>),
  Arguments(Vec<Expr>),
  Statement(Statement),
  LexSymbol(isize),
  Id(String),
  Module(Module),
  Typename(Typename),
  OptionExplicit(bool),
  VariableDeclaration(VariableDeclaration),
}

struct Visitor {
  stack_of_stack: Vec<VecDeque<ASTNode>>
}

// macro_rules! create_loc {
//   ($ctx:ident) => {
//     {
//       let start_line = $ctx.get_source_interval().a;
//       let start_column = $ctx.get_source_interval().a;
//       let stop_line = $ctx.get_source_interval().b;
//       let stop_column = $ctx.get_source_interval().b;
//       Loc {
//         start_line,
//         start_column,
//         stop_line,
//         stop_column,
//         text: $ctx.get_text().to_string()
//       }
//     }
//   }
// }

impl<'i> ParseTreeVisitor<'i, vbaParserContextType> for Visitor {
  fn visit_terminal(&mut self, node: &TerminalNode<'i, vbaParserContextType>) {    
    match node.symbol.get_token_type() {
      vbaparser::INT => {
        if let Cow::Borrowed(s) = node.symbol.text {
          let i = s.parse::<i32>().unwrap();
          self.return_node(ASTNode::Expr(Expr::Integer(i)));
        } else { panic!(); }
      },
      vbaparser::STRINGLITERAL => {
        if let Cow::Borrowed(s) = node.symbol.text {
          self.return_node(ASTNode::Expr(Expr::String(Rc::new(s[1..s.len()-1].to_owned()))));
        } else { panic!(); }
      },
      sym => {
        match sym {
          vbaparser::STAR | vbaparser::SLASH | vbaparser::PLUS | vbaparser::MINUS |
          vbaparser::GEQ | vbaparser::GT | vbaparser::LEQ | vbaparser::LT | vbaparser::EQUAL |
          vbaparser::NEQ | vbaparser::AND | vbaparser::OR | vbaparser::CONCAT |
          vbaparser::PUBLIC | vbaparser::PRIVATE | vbaparser::DIM => {
            self.return_node(ASTNode::LexSymbol(sym));
          },
          vbaparser::ID => {
            if let Cow::Borrowed(s) = node.symbol.text {
              self.return_node(ASTNode::Id(s.to_owned()));
            } else { panic!(); }
          }
          _ => {}
        };
      }
    }
  }
  
  fn visit_children(&mut self, node: &dyn vbaParserContext) {
    // println!("{:?}", self.stack_of_stack);
    self.visit_children_inner(node)
  }
}

macro_rules! extract {
  ($stack:ident, $pat:path) => {
    {
      match $stack.pop_front() {
        None => panic!("extract: stack empty"),
        Some(l) => match l {
          $pat(e) => e,
          _ => panic!("extract: pattern not matched: {:?}", l)
        }
      }
    }
  };
}

macro_rules! extract_opt {
  ($stack:ident, $pat:path) => {
    {
      match $stack.pop_front() {
        None => None,
        Some(l) => {
          match l {
            $pat(e) => Some(e),
            _ => {
              $stack.push_front(l);
              None
            }
          }
        }
      }
    }
  };
}

macro_rules! extract_opt_vec {
  ($stack:ident, $pat:path) => {
    {
      match $stack.front() {
        None => vec![],
        Some(l) => {
          match l {
            $pat(_) => {
              match $stack.pop_front().unwrap() { $pat(e) => e, e => panic!("extract: pattern not matched: {:?}", e) }
            },
            _ => vec![]
          }
        }
      }
    }
  };  
}

macro_rules! extract_list {
  ($stack:ident, $pat:path) => {
    {
      let mut args = vec![];
      loop {
        match $stack.front() {
          None => { break; }
          Some(e) => match e {
            $pat(_) => {
              let e = match $stack.pop_front().unwrap() { $pat(e) => e, _ => panic!() };
              args.push(e);
            },
            _ => { break; }
          }
        }
      }
      args
    }
  }
}

macro_rules! visit_rec {
  ($self:ident, $ctx:ident) => {
    {
      $self.stack_of_stack.push(VecDeque::new());
      $self.visit_children($ctx);
      $self.stack_of_stack.pop().unwrap()
    }
  }
}

fn retrieve_binary_operator(stack: &mut VecDeque<ASTNode>)-> (Expr, isize, Expr) {
  let e1 = extract!(stack, ASTNode::Expr);
  let op = extract!(stack, ASTNode::LexSymbol);
  let e2 = extract!(stack, ASTNode::Expr);
  (e1, op, e2)
}

impl Visitor {
  fn return_node(&mut self, node: ASTNode) {
    self.stack_of_stack.last_mut().unwrap().push_back(node);
  }
}

impl<'i> vbaVisitor<'i> for Visitor {
  fn visit_Chain_expr_chain(&mut self, ctx: &Chain_expr_chainContext<'i>) {
    let mut stack = visit_rec!(self, ctx);
    
    let e1 = extract!(stack, ASTNode::Chain);
    let e2 = extract!(stack, ASTNode::App);
    
    self.return_node(ASTNode::Chain(Chain::Method(Box::new(e1), e2)));
  }
  
  fn visit_Chain_expr_base(&mut self, ctx: &Chain_expr_baseContext<'i>) {
    let mut stack = visit_rec!(self, ctx);
    
    let e1 = extract!(stack, ASTNode::App);
    
    self.return_node(ASTNode::Chain(Chain::App(e1)));
  }
  
  fn visit_Expr_Chain(&mut self, ctx: &Expr_ChainContext<'i>) {
    let mut stack = visit_rec!(self, ctx);
    
    let e1 = extract!(stack, ASTNode::Chain);
    
    self.return_node(ASTNode::Expr(Expr::Var(e1)));
  }
  
  fn visit_Expr_New(&mut self, ctx: &Expr_NewContext<'i>) {
    let mut stack = visit_rec!(self, ctx);
    
    let id = extract!(stack, ASTNode::Id);
    
    self.return_node(ASTNode::Expr(Expr::New(Rc::new(id))));
  }
  
  fn visit_app_expr(&mut self, ctx: &App_exprContext<'i>) {
    let mut stack = visit_rec!(self, ctx);
    
    let name = extract!(stack, ASTNode::Id);
    let args = extract_opt_vec!(stack, ASTNode::Arguments);
    
    self.return_node(ASTNode::App(App {name: Rc::new(name), arguments: args}));
  }
  
  fn visit_module_variable_declaration(&mut self, ctx: &Module_variable_declarationContext<'i>) {
    let mut stack = visit_rec!(self, ctx);
    
    let variable_declaration = match extract!(stack, ASTNode::LexSymbol) {
      DIM => VariableDeclarationType::Dim,
      PUBLIC => VariableDeclarationType::Public,
      PRIVATE => VariableDeclarationType::Private,
      _ => unreachable!()
    };
    let id = extract!(stack, ASTNode::Id);
    let typename =
      match extract_opt!(stack, ASTNode::Typename) {
        Some(t) => t,
        None => Typename::Variant
      };
      
    self.return_node(ASTNode::VariableDeclaration(VariableDeclaration {declaration_type: variable_declaration, name: Rc::new(id), typename: typename}));
  }
  
  fn visit_module(&mut self, ctx: &ModuleContext<'i>) {
    let mut stack = visit_rec!(self, ctx);
    
    let option_explicit =
      match extract_opt!(stack, ASTNode::OptionExplicit) {
        None => false,
        Some(_) => true,
      };
    let mut functions = vec![];
    let mut fields = vec![];
    
    loop {
      match extract_opt!(stack, ASTNode::Function) {
        Some(f) => functions.push(f),
        None => match extract_opt!(stack, ASTNode::VariableDeclaration) {
          Some(f) => fields.push(f),
          None => break
        },
      };
    }
    
    self.return_node(ASTNode::Module(Module{ option_explicit, functions, fields }));
  }
  
  fn visit_option_explicit(&mut self, ctx: &Option_explicitContext<'i>) {
    let mut _stack = visit_rec!(self, ctx);
    
    self.return_node(ASTNode::OptionExplicit(true));
  }
  
  fn visit_param(&mut self, ctx: &ParamContext<'i>) {
    let mut stack = visit_rec!(self, ctx);
    
    let name = extract!(stack, ASTNode::Id);
    let typename =
      match extract_opt!(stack, ASTNode::Typename) {
        Some(t) => t,
        None => Typename::Variant
      };
    
    self.return_node(ASTNode::Param(Parameter {name: Rc::new(name), typename}));
    
  }
  
  fn visit_params(&mut self, ctx: &ParamsContext<'i>) {
    let mut stack = visit_rec!(self, ctx);
    
    let params = extract_list!(stack, ASTNode::Param);
    
    self.return_node(ASTNode::Params(params));
  }
  
  fn visit_function(&mut self, ctx: &FunctionContext<'i>) {
    let mut stack = visit_rec!(self, ctx);
    
    let modifier =
      match extract_opt!(stack, ASTNode::LexSymbol) {
        Some(m) => match m {
          PUBLIC  => Modifier::Public,
          PRIVATE => Modifier::Private,
          _ => unreachable!()
        },
        None => Modifier::Public
      };
    let name = extract!(stack, ASTNode::Id);
    let parameters = extract_opt_vec!(stack, ASTNode::Params);
    let return_typename: Typename =
      match extract_opt!(stack, ASTNode::Typename) {
        Some(e) => e,
        None => Typename::Variant
      };
    let body = extract!(stack, ASTNode::Block);
    let e = Function { modifier, function_type: FunctionType::Function, name: Rc::new(name), parameters, body: body, return_typename: return_typename };
    
    self.return_node(ASTNode::Function(e));
  }
  
  fn visit_procedure(&mut self, ctx: &ProcedureContext<'i>) {
    let mut stack = visit_rec!(self, ctx);
    
    let modifier =
      match extract_opt!(stack, ASTNode::LexSymbol) {
        Some(m) => match m {
          PUBLIC  => Modifier::Public,
          PRIVATE => Modifier::Private,
          _ => unreachable!()
        },
        None => Modifier::Public
      };
    let name = extract!(stack, ASTNode::Id);
    let parameters = extract_opt_vec!(stack, ASTNode::Params);
    let body = extract!(stack, ASTNode::Block);
    let e = Function { modifier, function_type: FunctionType::Procedure, name: Rc::new(name), parameters, body: body, return_typename: Typename::Variant };
    
    self.return_node(ASTNode::Function(e));
  }
  
  fn visit_property_let(&mut self, ctx: &Property_letContext<'i>) {
    let mut stack = visit_rec!(self, ctx);
    
    let modifier =
      match extract_opt!(stack, ASTNode::LexSymbol) {
        Some(m) => match m {
          PUBLIC  => Modifier::Public,
          PRIVATE => Modifier::Private,
          _ => unreachable!()
        },
        None => Modifier::Public
      };
    let name = extract!(stack, ASTNode::Id);
    let parameter = extract!(stack, ASTNode::Param);
    let body = extract!(stack, ASTNode::Block);
    let e = Function { modifier, function_type: FunctionType::PropertyLet, name: Rc::new(name), parameters: vec![parameter], body: body, return_typename: Typename::Variant };
    
    self.return_node(ASTNode::Function(e));
  }
  
  fn visit_block(&mut self, ctx: &BlockContext<'i>) {
    let mut stack = visit_rec!(self, ctx);
    
    let statements = extract_list!(stack, ASTNode::Statement);
    let e = Block { statements: statements };
    
    self.return_node(ASTNode::Block(e));
  }
  
  fn visit_Statement_Assign(&mut self, ctx: &Statement_AssignContext<'i>) {
    let mut stack = visit_rec!(self, ctx);
    
    let id1 = extract!(stack, ASTNode::Chain);
    extract!(stack, ASTNode::LexSymbol);
    let e1 = extract!(stack, ASTNode::Expr);
    
    self.return_node(ASTNode::Statement(Statement::Assign(AssignMode::Let, id1, e1)));
  }
  
  fn visit_Statement_SetAssign(&mut self, ctx: &Statement_SetAssignContext<'i>) {
    let mut stack = visit_rec!(self, ctx);
    
    let id1 = extract!(stack, ASTNode::Chain);
    extract!(stack, ASTNode::LexSymbol);
    let e1 = extract!(stack, ASTNode::Expr);
    
    self.return_node(ASTNode::Statement(Statement::Assign(AssignMode::Set, id1, e1)));
  }
  
  fn visit_Statement_Call(&mut self, ctx: &Statement_CallContext<'i>) {
    let mut stack = visit_rec!(self, ctx);
    
    let e1 = extract!(stack, ASTNode::Chain);
    let e2 = extract!(stack, ASTNode::Arguments);
    let e =
      match e1 {
        Chain::App(App {name, arguments: _}) => Chain::App(App {name, arguments: e2}),
        Chain::Method(chain, App {name, arguments: _}) => Chain::Method(chain, App {name, arguments: e2})
      };
    
    self.return_node(ASTNode::Statement(Statement::Expr(Expr::Var(e))));
  }
  
  fn visit_Statement_variable_declaration(&mut self, ctx: &Statement_variable_declarationContext<'i>) {
    let mut stack = visit_rec!(self, ctx);
    
    let variable_declaration = match extract!(stack, ASTNode::LexSymbol) {
      DIM => VariableDeclarationType::Dim,
      PUBLIC => VariableDeclarationType::Public,
      PRIVATE => VariableDeclarationType::Private,
      _ => unreachable!()
    };
    let id = extract!(stack, ASTNode::Id);
    let typename =
      if stack.len() == 0 {
        Typename::Variant
      } else {
        extract!(stack, ASTNode::Typename)
      };
    
    self.return_node(ASTNode::Statement(Statement::VariableDeclaration(VariableDeclaration{ declaration_type: variable_declaration, name: Rc::new(id), typename })));
  }
  
  fn visit_Statement_If(&mut self, ctx: &Statement_IfContext<'i>) {
    let mut stack = visit_rec!(self, ctx);
    
    let (expr, block1, block2) =
      if stack.len() == 2 {
        let expr = match stack.pop_front().unwrap() { ASTNode::Expr(e) => e, _ => panic!() };
        let block1 = match stack.pop_front().unwrap() { ASTNode::Block(e) => e, _ => panic!() };
        let block2 = None;
        (expr, block1, block2)
      } else {
        let expr = match stack.pop_front().unwrap() { ASTNode::Expr(e) => e, _ => panic!() };
        let block1 = match stack.pop_front().unwrap() { ASTNode::Block(e) => e, _ => panic!() };
        let block2 = Some (match stack.pop_front().unwrap() { ASTNode::Block(e) => e, _ => panic!() });
        (expr, block1, block2)
      };
    
    self.return_node(ASTNode::Statement(Statement::If(expr, block1, block2)));
  }
  
  fn visit_Statement_For(&mut self, ctx: &Statement_ForContext<'i>) {
    let mut stack = visit_rec!(self, ctx);
    
    let name = extract!(stack, ASTNode::Id);
    extract!(stack, ASTNode::LexSymbol);
    let expr1 = extract!(stack, ASTNode::Expr);
    let expr2 = extract!(stack, ASTNode::Expr);
    let block = extract!(stack, ASTNode::Block);
    match extract_opt!(stack, ASTNode::Id) {
      Some(name_) => if name_ != name { panic!("\"for\" variable mismatch"); },
      None => ()
    }
    
    self.return_node(ASTNode::Statement(Statement::For(Rc::new(name), expr1, expr2, block)));  
  }
  
  fn visit_Statement_DoWhile(&mut self, ctx: &Statement_DoWhileContext<'i>) {
    let mut stack = visit_rec!(self, ctx);
    
    let expr = extract!(stack, ASTNode::Expr);
    let block = extract!(stack, ASTNode::Block);
    
    self.return_node(ASTNode::Statement(Statement::DoWhile(expr, block)));
  }
  
  fn visit_Statement_Expr(&mut self, ctx: &Statement_ExprContext<'i>) {
    let mut stack = visit_rec!(self, ctx);
    
    let e = extract!(stack, ASTNode::Expr);
    
    self.return_node(ASTNode::Statement(Statement::Expr(e)));
  }
  
  fn visit_Expr_Add(&mut self, ctx: &Expr_AddContext<'i>) {    
    let mut stack = visit_rec!(self, ctx);
    let (e1, op, e2) = retrieve_binary_operator(&mut stack);
    let op = match op {
      PLUS => Op::Add,
      MINUS => Op::Sub,
      _ => panic!()
    };
    let expr = Expr::BinOp(op, Box::new(e1), Box::new(e2));
    self.return_node(ASTNode::Expr(expr));
  }

  fn visit_Expr_Mul(&mut self, ctx: &Expr_MulContext<'i>) {
    let mut stack = visit_rec!(self, ctx);
    let (e1, op, e2) = retrieve_binary_operator(&mut stack);
    let op = match op {
      STAR => Op::Mul,
      SLASH => Op::Div,
      _ => panic!()
    };
    let expr = Expr::BinOp(op, Box::new(e1), Box::new(e2));
    self.return_node(ASTNode::Expr(expr));
  }
  
  fn visit_Expr_Comp(&mut self, ctx: &Expr_CompContext<'i>) {
    let mut stack = visit_rec!(self, ctx);
    let (e1, op, e2) = retrieve_binary_operator(&mut stack);
    let op = match op {
      GEQ => Op::Geq,
      GT => Op::Gt,
      LEQ => Op::Leq,
      LT => Op::Lt,
      EQUAL => Op::Equal,
      NEQ => Op::Neq,
      _ => panic!()
    };
    let expr = Expr::BinOp(op, Box::new(e1), Box::new(e2));
    self.return_node(ASTNode::Expr(expr));
  }
  
  fn visit_Expr_Logical_Comb(&mut self, ctx: &Expr_Logical_CombContext<'i>) {
    let mut stack = visit_rec!(self, ctx);
    let (e1, op, e2) = retrieve_binary_operator(&mut stack);
    let op = match op {
      AND => Op::And,
      OR => Op::Or,
      _ => panic!()
    };
    let expr = Expr::BinOp(op, Box::new(e1), Box::new(e2));
    self.return_node(ASTNode::Expr(expr));
  }
  
  fn visit_Expr_Concat(&mut self, ctx: &Expr_ConcatContext<'i>) {
    let mut stack = visit_rec!(self, ctx);
    let (e1, op, e2) = retrieve_binary_operator(&mut stack);
    let op = match op {
      CONCAT => Op::Concat,
      _ => panic!()
    };
    let expr = Expr::BinOp(op, Box::new(e1), Box::new(e2));
    self.return_node(ASTNode::Expr(expr));
  }
  
  fn visit_arguments(&mut self, ctx: &ArgumentsContext<'i>) {
    let mut stack = visit_rec!(self, ctx);
    let arguments = extract_list!(stack, ASTNode::Expr);
    self.return_node(ASTNode::Arguments(arguments));
  }
  
  fn visit_Typename_Int(&mut self, ctx: &Typename_IntContext<'i>) {
    let mut _stack = visit_rec!(self, ctx);
    self.return_node(ASTNode::Typename(Typename::Integer));
  }
  fn visit_Typename_String(&mut self, ctx: &Typename_StringContext<'i>) {
    let mut _stack = visit_rec!(self, ctx);
    self.return_node(ASTNode::Typename(Typename::String));
  }
  fn visit_Typename_Variant(&mut self, ctx: &Typename_VariantContext<'i>) {
    let mut _stack = visit_rec!(self, ctx);
    self.return_node(ASTNode::Typename(Typename::Variant));
  }
  fn visit_Typename_Boolean(&mut self, ctx: &Typename_BooleanContext<'i>) {
    let mut _stack = visit_rec!(self, ctx);
    self.return_node(ASTNode::Typename(Typename::Boolean));
  }
  fn visit_Typename_Object(&mut self, ctx: &Typename_ObjectContext<'i>) {
    let mut _stack = visit_rec!(self, ctx);
    self.return_node(ASTNode::Typename(Typename::Object));
  }
  fn visit_Typename_Id(&mut self, ctx: &Typename_IdContext<'i>) {
    let mut stack = visit_rec!(self, ctx);
    
    let id = extract!(stack, ASTNode::Id);
    
    self.return_node(ASTNode::Typename(Typename::Id(Rc::new(id))));
  }
}

pub fn parse(filename: &str)-> Module {
  let contents = std::fs::read_to_string(filename)
      .expect("Something went wrong reading the file");
  let input = InputStream::new(&*contents);
  let lexer = vbalexer::vbaLexer::new(input);
  let token_source = CommonTokenStream::new(lexer);
  let mut parser = vbaparser::vbaParser::new(token_source);
  
  use std::io::Read;
  use gag::BufferRedirect;
  let mut buf = BufferRedirect::stderr().unwrap();
  
  let result = parser.startRule().expect("parser error");
  
  let mut output = String::new();
  buf.read_to_string(&mut output).unwrap();
  drop(buf);
  
  if !output.is_empty() {
    panic!("parse error (2): {}", output);
  }
  
  let mut visitor = Visitor{ stack_of_stack: vec![VecDeque::new()] };
  result.accept(&mut visitor);
  
  let mut r = visitor.stack_of_stack;
  // println!("FINAL: {:?}", r);
  match r.remove(0).pop_front().unwrap() {
    ASTNode::Module(m) => m,
    _ => panic!()
  }
}