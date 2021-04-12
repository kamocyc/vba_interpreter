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
// use antlr_rust::tree::ParseTree;

use std::borrow::Cow;

use crate::gen::ast::*;

#[derive(Debug, Clone)]
enum ASTNode {
  Expr(Expr),
  Chain(Chain),
  App(App),
  Function(Function),
  Block(Block),
  Params(Vec<Id>),
  Arguments(Vec<Expr>),
  Statement(Statement),
  LexSymbol(isize),
  Id(String),
  Module(Module),
}

struct Visitor {
  stack_of_stack: Vec<Vec<ASTNode>>
}

impl<'i> ParseTreeVisitor<'i, vbaParserContextType> for Visitor {
  fn visit_terminal(&mut self, node: &TerminalNode<'i, vbaParserContextType>) {
    match node.symbol.get_token_type() {
      vbaparser::INT => {
        if let Cow::Borrowed(s) = node.symbol.text {
          let i = s.parse::<i32>().unwrap();
          self.stack_of_stack.last_mut().unwrap().push(ASTNode::Expr(Expr::Int(i)));
        } else { panic!(); }
      },
      vbaparser::STRINGLITERAL => {
        if let Cow::Borrowed(s) = node.symbol.text {
          self.stack_of_stack.last_mut().unwrap().push(ASTNode::Expr(Expr::String(s[1..s.len()-1].to_owned())));
        } else { panic!(); }
      },
      sym => {
        match sym {
          vbaparser::STAR | vbaparser::SLASH | vbaparser::PLUS | vbaparser::MINUS |
          vbaparser::GEQ | vbaparser::GT | vbaparser::LEQ | vbaparser::LT | vbaparser::EQUAL |
          vbaparser::NEQ | vbaparser::AND | vbaparser::OR | vbaparser::CONCAT => {
            self.stack_of_stack.last_mut().unwrap().push(ASTNode::LexSymbol(sym));
          },
          vbaparser::ID => {
            if let Cow::Borrowed(s) = node.symbol.text {
              self.stack_of_stack.last_mut().unwrap().push(ASTNode::Id(s.to_owned()));
            } else { panic!(); }
          }
          _ => {}
        };
      }
    }
  }
  
  fn visit_children(&mut self, node: &dyn vbaParserContext) {
    println!("{:?}", self.stack_of_stack);
    self.visit_children_inner(node)
  }
}

macro_rules! extract {
  ($stack:ident, $pat:path) => {
    {
      assert!($stack.len() >= 1, "extract: stack empty");
      match $stack.pop().unwrap() { $pat(e) => e, e => panic!("extract: pattern not matched: {:?}", e) }
    }
  };
}

macro_rules! extract_opt_vec {
  ($stack:ident, $pat:path) => {
    {
      match $stack.last() {
        None => vec![],
        Some(l) => {
          match l {
            $pat(_) => {
              match $stack.pop().unwrap() { $pat(e) => e, e => panic!("extract: pattern not matched: {:?}", e) }
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
        match $stack.last() {
          None => { break; }
          Some(e) => match e {
            $pat(_) => {
              let e = match $stack.pop().unwrap() { $pat(e) => e, _ => panic!() };
              args.push(e);
            },
            _ => { break; }
          }
        }
      }
      args.reverse();
      args
    }
  }
}

fn retrieve_binary_operator(stack: &mut Vec<ASTNode>)-> (Expr, isize, Expr) {
  let e2 = extract!(stack, ASTNode::Expr);
  let op = extract!(stack, ASTNode::LexSymbol);
  let e1 = extract!(stack, ASTNode::Expr);
  (e1, op, e2)
}

impl<'i> vbaVisitor<'i> for Visitor {
  fn visit_Chain_expr_chain(&mut self, ctx: &Chain_expr_chainContext<'i>) {
    self.stack_of_stack.push(Vec::new()); 
    self.visit_children(ctx);
    let mut stack = self.stack_of_stack.pop().unwrap();
    
    let e2 = extract!(stack, ASTNode::App);
    let e1 = extract!(stack, ASTNode::Chain);
    self.stack_of_stack.last_mut().unwrap().push(ASTNode::Chain(Chain::Method(std::rc::Rc::new(e1), e2)));
  }
  
  fn visit_Chain_expr_base(&mut self, ctx: &Chain_expr_baseContext<'i>) {
    self.stack_of_stack.push(Vec::new()); 
    self.visit_children(ctx);
    let mut stack = self.stack_of_stack.pop().unwrap();
    
    let e1 = extract!(stack, ASTNode::App);
    self.stack_of_stack.last_mut().unwrap().push(ASTNode::Chain(Chain::App(e1)));
  }
  
  fn visit_Expr_Chain(&mut self, ctx: &Expr_ChainContext<'i>) {
    self.stack_of_stack.push(Vec::new()); 
    self.visit_children(ctx);
    let mut stack = self.stack_of_stack.pop().unwrap();
    
    let e1 = extract!(stack, ASTNode::Chain);
    self.stack_of_stack.last_mut().unwrap().push(ASTNode::Expr(Expr::Var(e1)));
  }
  
  fn visit_app_expr(&mut self, ctx: &App_exprContext<'i>) {
    self.stack_of_stack.push(Vec::new()); 
    self.visit_children(ctx);
    let mut stack = self.stack_of_stack.pop().unwrap();
    
    let args = extract_opt_vec!(stack, ASTNode::Arguments);
    let id = extract!(stack, ASTNode::Id);
    self.stack_of_stack.last_mut().unwrap().push(ASTNode::App(App {id: id, arguments: args}));
  }
  
  fn visit_module(&mut self, ctx: &ModuleContext<'i>) {
    self.stack_of_stack.push(Vec::new()); 
    self.visit_children(ctx);
    let mut stack = self.stack_of_stack.pop().unwrap();
    
    let functions = extract_list!(stack, ASTNode::Function);
    self.stack_of_stack.last_mut().unwrap().push(ASTNode::Module(Module{ functions }));
  }
  
  fn visit_params(&mut self, ctx: &ParamsContext<'i>) {
    self.stack_of_stack.push(Vec::new()); 
    self.visit_children(ctx);
    let mut stack = self.stack_of_stack.pop().unwrap();
    
    let params = extract_list!(stack, ASTNode::Id);
    self.stack_of_stack.last_mut().unwrap().push(ASTNode::Params(params));
  }
  
  fn visit_function(&mut self, ctx: &FunctionContext<'i>) {
    self.stack_of_stack.push(Vec::new()); 
    self.visit_children(ctx);
    let mut stack = self.stack_of_stack.pop().unwrap();
    
    let body = extract!(stack, ASTNode::Block);
    let parameters = extract_opt_vec!(stack, ASTNode::Params);
    let id = extract!(stack, ASTNode::Id);
    
    let e = Function { id, parameters, body: FunctionBody::VBA(body) };
    self.stack_of_stack.last_mut().unwrap().push(ASTNode::Function(e));
  }
  
  fn visit_block(&mut self, ctx: &BlockContext<'i>) {
    self.stack_of_stack.push(Vec::new());
    self.visit_children(ctx);
    let mut stack = self.stack_of_stack.pop().unwrap();
    
    let statements = extract_list!(stack, ASTNode::Statement);
    let e = Block { statements: statements };
    self.stack_of_stack.last_mut().unwrap().push(ASTNode::Block(e));
  }
  
  fn visit_Statement_Assign(&mut self, ctx: &Statement_AssignContext<'i>) {
    self.stack_of_stack.push(Vec::new());
    self.visit_children(ctx);
    let mut stack = self.stack_of_stack.pop().unwrap();
    
    let e1 = extract!(stack, ASTNode::Expr);
    extract!(stack, ASTNode::LexSymbol);
    let id1 = extract!(stack, ASTNode::Chain);
    self.stack_of_stack.last_mut().unwrap().push(ASTNode::Statement(Statement::Assign(id1, e1)));
  }
  
  fn visit_Statement_If(&mut self, ctx: &Statement_IfContext<'i>) {
    self.stack_of_stack.push(Vec::new());
    self.visit_children(ctx);
    let mut stack = self.stack_of_stack.pop().unwrap();
    let (expr, block1, block2) =
      if stack.len() == 2 {
        let block2 = None;
        let block1 = match stack.pop().unwrap() { ASTNode::Block(e) => e, _ => panic!() };
        let expr = match stack.pop().unwrap() { ASTNode::Expr(e) => e, _ => panic!() };
        (expr, block1, block2)
      } else {
        let block2 = Some (match stack.pop().unwrap() { ASTNode::Block(e) => e, _ => panic!() });
        let block1 = match stack.pop().unwrap() { ASTNode::Block(e) => e, _ => panic!() };
        let expr = match stack.pop().unwrap() { ASTNode::Expr(e) => e, _ => panic!() };
        (expr, block1, block2)
      };
    
    self.stack_of_stack.last_mut().unwrap().push(ASTNode::Statement(Statement::If(expr, block1, block2)));
  }
  
  fn visit_Statement_For(&mut self, ctx: &Statement_ForContext<'i>) {
    self.stack_of_stack.push(Vec::new());
    self.visit_children(ctx);
    let mut stack = self.stack_of_stack.pop().unwrap();
    let block = extract!(stack, ASTNode::Block);
    let expr2 = extract!(stack, ASTNode::Expr);
    let expr1 = extract!(stack, ASTNode::Expr);
    extract!(stack, ASTNode::LexSymbol);
    let id = extract!(stack, ASTNode::Id);
    self.stack_of_stack.last_mut().unwrap().push(ASTNode::Statement(Statement::For(id, expr1, expr2, block)));  
  }
  
  fn visit_Statement_Expr(&mut self, ctx: &Statement_ExprContext<'i>) {
    self.stack_of_stack.push(Vec::new());
    self.visit_children(ctx);
    let mut stack = self.stack_of_stack.pop().unwrap();
    let e = extract!(stack, ASTNode::Expr);
    self.stack_of_stack.last_mut().unwrap().push(ASTNode::Statement(Statement::Expr(e)));
  }
  
  fn visit_Expr_Add(&mut self, ctx: &Expr_AddContext<'i>) {    
    self.stack_of_stack.push(Vec::new());
    self.visit_children(ctx);
    let mut stack = self.stack_of_stack.pop().unwrap();
    let (e1, op, e2) = retrieve_binary_operator(&mut stack);
    let op = match op {
      PLUS => Op::Add,
      MINUS => Op::Sub,
      _ => panic!()
    };
    let expr = Expr::BinOp(op, std::rc::Rc::new(e1), std::rc::Rc::new(e2));
    self.stack_of_stack.last_mut().unwrap().push(ASTNode::Expr(expr));
  }

  fn visit_Expr_Mul(&mut self, ctx: &Expr_MulContext<'i>) {
    self.stack_of_stack.push(Vec::new());
    self.visit_children(ctx);
    let mut stack = self.stack_of_stack.pop().unwrap();
    let (e1, op, e2) = retrieve_binary_operator(&mut stack);
    let op = match op {
      STAR => Op::Mul,
      SLASH => Op::Div,
      _ => panic!()
    };
    let expr = Expr::BinOp(op, std::rc::Rc::new(e1), std::rc::Rc::new(e2));
    self.stack_of_stack.last_mut().unwrap().push(ASTNode::Expr(expr));
  }
  
  fn visit_Expr_Comp(&mut self, ctx: &Expr_CompContext<'i>) {
    self.stack_of_stack.push(Vec::new());
    self.visit_children(ctx);
    let mut stack = self.stack_of_stack.pop().unwrap();
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
    let expr = Expr::BinOp(op, std::rc::Rc::new(e1), std::rc::Rc::new(e2));
    self.stack_of_stack.last_mut().unwrap().push(ASTNode::Expr(expr));
  }
  
  fn visit_Expr_Logical_Comb(&mut self, ctx: &Expr_Logical_CombContext<'i>) {
    self.stack_of_stack.push(Vec::new());
    self.visit_children(ctx);
    let mut stack = self.stack_of_stack.pop().unwrap();
    let (e1, op, e2) = retrieve_binary_operator(&mut stack);
    let op = match op {
      AND => Op::And,
      OR => Op::Or,
      _ => panic!()
    };
    let expr = Expr::BinOp(op, std::rc::Rc::new(e1), std::rc::Rc::new(e2));
    self.stack_of_stack.last_mut().unwrap().push(ASTNode::Expr(expr));
  }
  
  fn visit_Expr_Concat(&mut self, ctx: &Expr_ConcatContext<'i>) {
    self.stack_of_stack.push(Vec::new());
    self.visit_children(ctx);
    let mut stack = self.stack_of_stack.pop().unwrap();
    let (e1, op, e2) = retrieve_binary_operator(&mut stack);
    let op = match op {
      CONCAT => Op::Concat,
      _ => panic!()
    };
    let expr = Expr::BinOp(op, std::rc::Rc::new(e1), std::rc::Rc::new(e2));
    self.stack_of_stack.last_mut().unwrap().push(ASTNode::Expr(expr));
  }
  
  fn visit_arguments(&mut self, ctx: &ArgumentsContext<'i>) {
    self.stack_of_stack.push(Vec::new());
    self.visit_children(ctx);
    let mut stack = self.stack_of_stack.pop().unwrap();
    let arguments = extract_list!(stack, ASTNode::Expr);
    self.stack_of_stack.last_mut().unwrap().push(ASTNode::Arguments(arguments));
  }
}

pub fn parse(filename: &str)-> Module {
  let contents = std::fs::read_to_string(filename)
      .expect("Something went wrong reading the file");
  let input = InputStream::new(&*contents);
  let lexer = vbalexer::vbaLexer::new(input);
  let token_source = CommonTokenStream::new(lexer);
  let mut parser = vbaparser::vbaParser::new(token_source);    
  let result = parser.startRule().expect("parser error");
  
  let mut visitor = Visitor{ stack_of_stack: vec![Vec::new()] };
  result.accept(&mut visitor);
  
  let r = visitor.stack_of_stack;
  println!("{:?}", r);
  match r[0][0].clone() {
    ASTNode::Module(m) => m
    ,_ => panic!()
  }
}