use std::rc::Rc;

#[derive(Debug, Clone)]
pub struct Module {
  pub functions: Vec<Function>
}

#[derive(Debug, Clone)]
pub struct Function {
  pub id: Id,
  pub parameters: Vec<Id>,
  pub body: Block,
}

#[derive(Debug, Clone)]
pub struct Block {
  pub statements: Vec<Statement>
}

#[derive(Debug, Clone)]
pub enum Statement {
  Assign(Id, Expr),
  Expr(Expr),
}

pub type Id = String;

#[derive(Debug, Clone)]
pub enum Expr {
  Int(i32),
  Add(Rc<Expr>, Rc<Expr>),
  Sub(Rc<Expr>, Rc<Expr>),
  Mul(Rc<Expr>, Rc<Expr>),
  Div(Rc<Expr>, Rc<Expr>),
}
