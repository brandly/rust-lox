use crate::expr::Expr;

pub enum Stmt {
    Expression(Expr),
    Print(Expr),
}

impl Stmt {
    pub fn eval(&self) {
        match &self {
            Stmt::Expression(expr) => {
                expr.eval();
                ()
            }
            Stmt::Print(expr) => {
                println!("{:?}", expr.eval());
                ()
            }
        }
    }
}
