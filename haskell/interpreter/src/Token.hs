module Token (Token (..), TokenType (..)) where

newtype Token = Token TokenType
    deriving (Show, Eq)

data TokenType
    = Illegal
    | Eof
    | -- Idenfitieres
      Ident String
    | Int Int
    | -- Operators
      Assign
    | Plus
    | -- Delimeters
      Comma
    | Semicolon
    | LParen
    | RParen
    | LBrace
    | RBrace
    | -- Keywords
      Function
    | Let
    deriving (Show, Eq)
