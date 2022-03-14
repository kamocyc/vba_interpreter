// use std::rc::Rc;

pub type Id = String;

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
  Assign(Chain, Expr),
  If(Expr, Block, Option<Block>),
  For(Id, Expr, Expr, Block),
  Expr(Expr),
}

#[derive(Debug, Clone)]
pub enum Expr {
  Int(i32),
  String(String),
  Var(Chain),
  BinOp(Op, Box<Expr>, Box<Expr>)
}

#[derive(Debug, Clone)]
pub enum Op {
  Add,
  Sub,
  Mul,
  Div,
  Geq,
  Gt,
  Leq,
  Lt,
  Equal,
  Neq,
  And,
  Or,
  Concat,
}

#[derive(Debug, Clone)]
pub enum Chain {
  App(App),
  Method(Box<Chain>, App)
}

#[derive(Debug, Clone)]
pub struct App {
  pub id: Id,
  pub arguments: Vec<Expr>
}
