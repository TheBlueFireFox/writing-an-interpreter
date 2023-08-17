module Token (TokenType (..)) where

import Data.Int (Int64)

data TokenType
    = Illegal String
    | Eof
    | -- Idenfitieres
      Ident String
    | Int Int64
    | Str String
    | -- Operators
      Assign
    | Plus
    | Minus
    | Bang
    | Asterisk
    | Slash
    | Lt
    | Gt
    | Eq
    | NotEq
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
    | KTrue
    | KFalse
    | If
    | Else
    | Return
    deriving (Show, Eq)
