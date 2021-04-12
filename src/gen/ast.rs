use std::rc::Rc;

#[derive(Debug, Clone)]
pub struct Module {
  pub functions: Vec<Function>
}

#[derive(Debug, Clone)]
pub struct Function {
  pub id: Id,
  pub parameters: Vec<Id>,
  pub body: FunctionBody,
}

#[derive(Clone)]
pub enum FunctionBody {
  Native(fn(&Vec<crate::runtime::Value>) -> crate::runtime::Value),
  VBA(Block)
}

impl std::fmt::Debug for FunctionBody {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    f.debug_tuple("").finish()
  }
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

pub type Id = String;

#[derive(Debug, Clone)]
pub enum Expr {
  Int(i32),
  String(String),
  Var(Chain),
  BinOp(Op, Rc<Expr>, Rc<Expr>)
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
  Method(Rc<Chain>, App)
}

#[derive(Debug, Clone)]
pub struct App {
  pub id: Id,
  pub arguments: Vec<Expr>
}
