use std::rc::Rc;

// TODO: deriveマクロを使って、ASTの元となる構文情報を付与したい
pub type Id = Rc<String>;

fn append_vec<T>(v1: Vec<T>, v2: Vec<T>)-> Vec<T> {
  v1.into_iter().chain(v2).collect()
}

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

#[derive(Debug, Clone, PartialEq)]
pub enum FunctionType { Function, Procedure }

#[derive(Debug, Clone)]
pub struct Function {
  pub modifier: Modifier,
  pub function_type: FunctionType,
  pub name: Id,
  pub parameters: Vec<Parameter>,
  pub body: Block,
  pub return_typename: Typename
}

#[derive(Debug, Clone)]
pub struct Block {
  pub statements: Vec<Statement>
}

impl Block {
  pub fn get_variables(&self)-> Vec<Id> {
    self.statements.iter().map(|x| x.get_variables()).collect::<Vec<Vec<Id>>>().concat()
  }
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
  VariableDeclaration(VariableDeclarationType, Id, Typename)
}

impl Statement {
  pub fn get_variables(&self)-> Vec<Id> {
    match self {
      Statement::Assign(chain, expr) => append_vec(chain.get_variables(), expr.get_variables()),
      Statement::If(expr, then_c, else_c) =>
        append_vec(
          append_vec(expr.get_variables(), then_c.get_variables()),
          match else_c {
            Some(l) => l.get_variables(),
            None => vec![]
          }),
      Statement::For(id, e1, e2, block) =>
        append_vec(append_vec(append_vec(vec![Rc::clone(id)], e1.get_variables()), e2.get_variables()), block.get_variables()),
      Statement::Expr(e) => e.get_variables(),
      Statement::VariableDeclaration(_, id, _) => vec![Rc::clone(id)]
    }
  }
}

#[derive(Debug, Clone)]
pub enum Expr {
  Int(i32),
  String(Rc<String>),
  Var(Chain),
  BinOp(Op, Box<Expr>, Box<Expr>)
}

impl Expr {
  pub fn get_variables(&self)-> Vec<Id> {
    match self {
      Expr::Int(_) => vec![],
      Expr::String(_) => vec![],
      Expr::Var(chain) => chain.get_variables(),
      Expr::BinOp(_, e1, e2) => append_vec(e1.get_variables(), e2.get_variables()),
    }
  }
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

impl Chain {
  pub fn get_variables(&self)-> Vec<Id> {
    match self {
      Chain::App(app) => app.get_variables(false),
      Chain::Method(chain, app) => append_vec(chain.get_variables(), app.get_variables(true))
    }
  }
}

#[derive(Debug, Clone)]
pub struct App {
  pub name: Id,
  pub arguments: Vec<Expr>
}

impl App {
  pub fn get_variables(&self, expr_only: bool)-> Vec<Id> {
    append_vec(
      if expr_only { vec![] } else { vec![Rc::clone(&self.name)] },
      self.arguments.iter().map(|x| x.get_variables()).collect::<Vec<Vec<Id>>>().concat())
  }
}
