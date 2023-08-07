use std::fmt::Display;

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Program<'input> {
    pub statements: Vec<Statement<'input>>,
}

impl Display for Program<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for statement in &self.statements {
            write!(f, "{statement}")?;
        }
        Ok(())
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Statement<'input> {
    Let(&'input str, Expression<'input>),
    Return(Expression<'input>),
    Expression(Expression<'input>),
    Block(Vec<Statement<'input>>),
    Invalid,
}

impl Display for Statement<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Statement::Let(name, expr) => write!(f, "let {name} = {expr};"),
            Statement::Return(expr) => write!(f, "return {expr};"),
            Statement::Expression(expr) => write!(f, "{expr}"),
            Statement::Invalid => write!(f, "invalid Statement -.-;"),
            Statement::Block(exprs) => {
                for expr in exprs {
                    write!(f, "{expr}")?;
                }
                Ok(())
            }
        }
    }
}

pub type BS<'i> = Box<Statement<'i>>;
pub type BE<'i> = Box<Expression<'i>>;

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Expression<'input> {
    Ident(&'input str),
    Int(i64),
    Bool(bool),
    Neg(BE<'input>),
    Not(BE<'input>),
    Add(BE<'input>, BE<'input>),
    Min(BE<'input>, BE<'input>),
    Mul(BE<'input>, BE<'input>),
    Div(BE<'input>, BE<'input>),
    Neq(BE<'input>, BE<'input>),
    Eq(BE<'input>, BE<'input>),
    Gt(BE<'input>, BE<'input>),
    Le(BE<'input>, BE<'input>),
    If(BE<'input>, BE<'input>, Option<BE<'input>>),
    Fn(Vec<Expression<'input>>, BE<'input>),
    Call(BE<'input>, Vec<Expression<'input>>),
    Invalid,
}

impl Display for Expression<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let single = |f: &mut std::fmt::Formatter<'_>, sym, expr| write!(f, "({}{})", sym, expr);
        let two = |f: &mut std::fmt::Formatter<'_>, sym, l, r| write!(f, "({} {} {})", l, sym, r);
        let param = |p: &[Expression<'_>]| {
            p.iter()
                .map(|expr| format!("{expr}"))
                .collect::<Vec<_>>()
                .join(", ")
        };
        let alt_cond = |alt: &Option<BE<'_>>| {
            alt.as_ref()
                .map(|c| format!("else {c}"))
                .unwrap_or_default()
        };

        match self {
            Expression::Ident(lit) => write!(f, "{lit}"),
            Expression::Int(lit) => write!(f, "{lit}"),
            Expression::Bool(lit) => write!(f, "{lit}"),
            Expression::Neg(expr) => single(f, "-", expr),
            Expression::Not(expr) => single(f, "!", expr),
            Expression::Add(l, r) => two(f, "+", l, r),
            Expression::Min(l, r) => two(f, "-", l, r),
            Expression::Mul(l, r) => two(f, "*", l, r),
            Expression::Div(l, r) => two(f, "/", l, r),
            Expression::Neq(l, r) => two(f, "!=", l, r),
            Expression::Eq(l, r) => two(f, "==", l, r),
            Expression::Gt(l, r) => two(f, ">", l, r),
            Expression::Le(l, r) => two(f, "<", l, r),
            Expression::If(cond, cons, alt) => write!(f, "if {cond} {cons}{}", alt_cond(alt)),
            Expression::Fn(params, blk) => write!(f, "fn({}){}", param(params), blk),
            Expression::Call(fun, params) => write!(f, "{}({})", fun, param(params)),
            Expression::Invalid => write!(f, "InvalidExpression"),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_display_let() {
        let program = Program {statements : vec![
            Statement::Let("myVar", Expression::Ident("anotherVar"))
        ]};
        let program = format!("{program}");

        assert_eq!("let myVar = anotherVar;", program);
    }
}

