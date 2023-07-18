module Ast (module Ast) where

import Token (TokenType)

newtype Program = Program [Statement]
    deriving (Show, Eq)

data Statement
    = LetStatement String Expression
    | ReturnStatement Expression
    | IfStatement Expression
    | InvalidStatement
    deriving (Show, Eq)

data Expression
    = IdentExpr TokenType String
    | Invalid
    deriving (Show, Eq)
