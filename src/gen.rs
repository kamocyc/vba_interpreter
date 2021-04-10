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

use std::borrow::Cow;

use crate::gen::ast::*;

#[derive(Debug, Clone)]
enum ASTNode {
  Expr(Expr),
  Function(Function),
  Block(Block),
  Params(Vec<Id>),
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
      sym => {
        match sym {
          vbaparser::STAR | vbaparser::SLASH | vbaparser::PLUS | vbaparser::MINUS => {
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

fn retrieve_binary_operator(stack: &mut Vec<ASTNode>)-> (Expr, isize, Expr) {
  let e2 = match stack.pop().unwrap() { ASTNode::Expr(e) => e, _ => panic!() };
  let op = match stack.pop().unwrap() { ASTNode::LexSymbol(l) => l, _ => panic!() };
  let e1 = match stack.pop().unwrap() { ASTNode::Expr(e) => e, _ => panic!() };
  (e1, op, e2)
}

impl<'i> vbaVisitor<'i> for Visitor {
  fn visit_module(&mut self, ctx: &ModuleContext<'i>) {
    self.stack_of_stack.push(Vec::new()); 
    self.visit_children(ctx);
    let stack = self.stack_of_stack.pop().unwrap();
    let functions: Vec<Function> = stack.into_iter().map(|node|
      match node {
        ASTNode::Function(f) => f,
        _ => panic!()
      }
    ).collect();
    
    self.stack_of_stack.last_mut().unwrap().push(ASTNode::Module(Module{ functions }));
  }
  
  fn visit_params(&mut self, ctx: &ParamsContext<'i>) {
    self.stack_of_stack.push(Vec::new()); 
    self.visit_children(ctx);
    let stack = self.stack_of_stack.pop().unwrap();
    let params: Vec<Id> = stack.into_iter().map(|node|
      match node {
        ASTNode::Id(s) => s,
        _ => panic!()
      }
    ).collect();
    self.stack_of_stack.last_mut().unwrap().push(ASTNode::Params(params));
  }
  
  fn visit_function(&mut self, ctx: &FunctionContext<'i>) {
    self.stack_of_stack.push(Vec::new()); 
    self.visit_children(ctx);
    let mut stack = self.stack_of_stack.pop().unwrap();
    let body = match stack.pop().unwrap() { ASTNode::Block(e) => e, _ => panic!() };
    let (id, parameters) =
      match stack.pop().unwrap() {
        ASTNode::Params(ps) => {
          match stack.pop().unwrap() {
            ASTNode::Id(id) => (id, ps),
            _ => panic!()
          }
        },
        ASTNode::Id(id) => (id, vec![]),
        _ => panic!()
      };
    let e = Function { id, parameters, body };
    self.stack_of_stack.last_mut().unwrap().push(ASTNode::Function(e));
  }
  
  fn visit_block(&mut self, ctx: &BlockContext<'i>) {
    self.stack_of_stack.push(Vec::new());
    self.visit_children(ctx);
    let stack = self.stack_of_stack.pop().unwrap();
    let statements: Vec<Statement> = stack.into_iter().map(|node|
      match node {
        ASTNode::Statement(s) => s,
        _ => panic!()
      }
    ).collect();
    let e = Block { statements: statements };
    self.stack_of_stack.last_mut().unwrap().push(ASTNode::Block(e));
  }
  
  fn visit_Statement_Expr(&mut self, ctx: &Statement_ExprContext<'i>) {
    self.stack_of_stack.push(Vec::new());
    self.visit_children(ctx);
    let mut stack = self.stack_of_stack.pop().unwrap();
    let e = match stack.pop().unwrap() { ASTNode::Expr(e) => e, _ => panic!() };
    self.stack_of_stack.last_mut().unwrap().push(ASTNode::Statement(Statement::Expr(e)));
  }
  
  fn visit_Expr_Add(&mut self, ctx: &Expr_AddContext<'i>) {
    // println!("visit expr_add");
    self.stack_of_stack.push(Vec::new());
    self.visit_children(ctx);
    // println!("visit (2) expr_add");
    // println!("{:?}", self.stack_of_stack);
    let mut stack = self.stack_of_stack.pop().unwrap();
    let (e1, op, e2) = retrieve_binary_operator(&mut stack);
    let expr = match op {
      PLUS => Expr::Add(std::rc::Rc::new(e1), std::rc::Rc::new(e2)),
      MINUS => Expr::Sub(std::rc::Rc::new(e1), std::rc::Rc::new(e2)),
      _ => panic!()
    };
    self.stack_of_stack.last_mut().unwrap().push(ASTNode::Expr(expr));
  }

  fn visit_Expr_Mul(&mut self, ctx: &Expr_MulContext<'i>) {
    // println!("visit expr_add");
    self.stack_of_stack.push(Vec::new());
    self.visit_children(ctx);
    // println!("visit (2) expr_add");
    // println!("{:?}", self.stack_of_stack);
    let mut stack = self.stack_of_stack.pop().unwrap();
    let (e1, op, e2) = retrieve_binary_operator(&mut stack);
    let expr = match op {
      STAR => Expr::Mul(std::rc::Rc::new(e1), std::rc::Rc::new(e2)),
      SLASH => Expr::Div(std::rc::Rc::new(e1), std::rc::Rc::new(e2)),
      _ => panic!()
    };
    self.stack_of_stack.last_mut().unwrap().push(ASTNode::Expr(expr));
  }
  
  // fn visit_Expr_Int(&mut self, ctx: &Expr_IntContext<'i>) {
  //   println!("visit int");
  // }
}

pub fn data()-> Module {
  let filename = "test/test1.bas";
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