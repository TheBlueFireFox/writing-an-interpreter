#[derive(Debug, PartialEq, Eq)]
pub struct Program<'input> {
    pub statements: Vec<Statement<'input>>,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Statement<'input> {
    Let(&'input str, Expression<'input>),
    Return(Expression<'input>),
    Expression(Expression<'input>),
    Block(Vec<Statement<'input>>),
    Invalid,
}

pub type BS<'i> = Box<Statement<'i>>;
pub type BE<'i> = Box<Expression<'i>>;

#[derive(Debug, PartialEq, Eq)]
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
