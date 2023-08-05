#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Token<'input> {
    Illegal(u8),
    Eof,

    // Identifierts + literals
    Ident(&'input str),
    Int(i64),

    // Operators
    Assign,
    Plus,
    Minus,
    Bang,
    Asterisk,
    Slash,

    Lt,
    Gt,

    Eq,
    Neq,

    // Delimiters
    Comma,
    Semicolon,

    LBrace,
    RBrace,
    LParen,
    RParen,
    
    // keywords
    Fun,
    Let,
    False,
    True,
    Return,
    If,
    Else,
}
