#![feature(try_blocks)]
#![allow(unused_braces)]

mod gen;

mod runtime {
  use crate::gen::ast::*;
  
  struct Runtime {
    program: Module,
  }
  
  impl Runtime {
    fn evaluate_program(&self)-> i32 {
      let main_functions = self.program.functions.iter().filter(|f| f.id == "main").collect::<Vec<&Function>>();
      if main_functions.len() == 1 {
        let main_function = main_functions[0];
        self.evaluate_block(&main_function.body)
      } else {
        panic!("should have one main function");
      }
    }
    
    fn evaluate_block(&self, expr: &Block)-> i32 {
      let mut temp = 0;
      for statement in &expr.statements {
        match statement {
          Statement::Expr(expr) => {
            temp = self.evaluate_expr(&expr);
          },
          _ => panic!()
        }
      }
      
      temp
    }
    
    fn evaluate_expr(&self, expr: &Expr)-> i32 {
      match expr {
        Expr::Add(e1, e2) => {
          self.evaluate_expr(e1) + self.evaluate_expr(e2)
        },
        Expr::Sub(e1, e2) => {
          self.evaluate_expr(e1) - self.evaluate_expr(e2)
        },
        Expr::Mul(e1, e2) => {
          self.evaluate_expr(e1) * self.evaluate_expr(e2)
        },
        Expr::Div(e1, e2) => {
          self.evaluate_expr(e1) / self.evaluate_expr(e2)
        },
        Expr::Int(i) => {
          *i
        }
      }
    } 
  }
  
  pub fn foo()-> i32 {
    // crate::gen::data();
    let runtime = Runtime { program: crate::gen::data() };
    runtime.evaluate_program()
  }
}

fn main() {
  println!("result: {}", runtime::foo());
}
