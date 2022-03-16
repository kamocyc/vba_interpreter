use std::rc::Rc;

pub type Id = Rc<String>;

#[derive(Debug, Clone)]
pub struct Module {
  pub functions: Vec<Function>
}

#[derive(Debug, Clone)]
pub enum Modifier {
  Public,
  Private
}

#[derive(Debug, Clone)]
pub struct Parameter {
  pub name: Id,
  pub typename: Typename
}

#[derive(Debug, Clone)]
pub struct Function {
  pub modifier: Modifier,
  pub name: Id,
  pub parameters: Vec<Parameter>,
  pub body: Block,
  pub return_typename: Typename
}

#[derive(Debug, Clone)]
pub struct Block {
  pub statements: Vec<Statement>
}

#[derive(Debug, Clone)]
pub enum Typename {
  Integer,
  Boolean,
  String,
  Variant
}

#[derive(Debug, Clone)]
pub enum VariableDeclarationType {
  Dim, Public, Private
}

#[derive(Debug, Clone)]
pub enum Statement {
  Assign(Chain, Expr),
  If(Expr, Block, Option<Block>),
  For(Id, Expr, Expr, Block),
  Expr(Expr),
  VariableDeclaration(VariableDeclarationType, Typename)
}

#[derive(Debug, Clone)]
pub enum Expr {
  Int(i32),
  String(Rc<String>),
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
  pub name: Id,
  pub arguments: Vec<Expr>
}
