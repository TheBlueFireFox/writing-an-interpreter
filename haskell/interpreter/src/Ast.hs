module Ast (Program (..), Statement (..), Expression (..)) where

import Data.Int (Int64)
import Data.List (intercalate)

newtype Program = Program [Statement]
    deriving (Eq)

instance Show Program where
    show (Program statements) = intercalate "" $ map show statements

data Statement
    = LetStatement String Expression
    | ReturnStatement Expression
    | ExpressionStatement Expression
    | BlockStatement [Statement]
    | InvalidStatement
    deriving (Eq)

instance Show Statement where
    show (LetStatement name expr) = "let " ++ name ++ " = " ++ show expr ++ ";"
    show (ReturnStatement expr) = "return " ++ show expr ++ ";"
    show (ExpressionStatement expr) = show expr
    show (BlockStatement exprs) = intercalate "" $ map show exprs
    show InvalidStatement = "InvalidStatement -.-;"

data Expression
    = IdentExpr String
    | IntegerExpr Int64
    | BooleanExpr Bool
    | NegExpr Expression
    | NotExpr Expression
    | AddExpr Expression Expression
    | MinExpr Expression Expression
    | MulExpr Expression Expression
    | DivExpr Expression Expression
    | NeqExpr Expression Expression
    | EqExpr Expression Expression
    | GtExpr Expression Expression
    | LeExpr Expression Expression
    | IfExpr Expression Statement (Maybe Statement) -- Condition BlockStatement BlockStatement
    | FnExpr [Expression] Statement -- Params BlockStatement 
    | Invalid
    deriving (Eq)

showHelperSingle :: (Show a) => String -> a -> String
showHelperSingle sym expr = "(" ++ sym ++ show expr ++ ")"

showHelperTwo :: (Show a1, Show a2) => String -> a1 -> a2 -> String
showHelperTwo sym l r = "(" ++ show l ++ " " ++ sym ++ " " ++ show r ++ ")"

showHelperIf :: (Show a, Show p1, Show p2) => p1 -> p2 -> Maybe a -> [Char]
showHelperIf cond cons alt =
    let
        hasAlt (Just altBlock) = "else " ++ show altBlock
        hasAlt Nothing = ""
        base = "if " ++ show cond ++ " " ++ show cons
     in
        base ++ hasAlt alt

instance Show Expression where
    show (IdentExpr lit) = lit
    show (IntegerExpr lit) = show lit
    show (BooleanExpr True) = "true"
    show (BooleanExpr False) = "false"
    show (NegExpr expr) = showHelperSingle "-" expr
    show (NotExpr expr) = showHelperSingle "!" expr
    show (AddExpr l r) = showHelperTwo "+" l r
    show (MinExpr l r) = showHelperTwo "-" l r
    show (MulExpr l r) = showHelperTwo "*" l r
    show (DivExpr l r) = showHelperTwo "/" l r
    show (NeqExpr l r) = showHelperTwo "!=" l r
    show (EqExpr l r) = showHelperTwo "==" l r
    show (GtExpr l r) = showHelperTwo ">" l r
    show (LeExpr l r) = showHelperTwo "<" l r
    show (IfExpr cond cons alt) = showHelperIf cond cons alt
    show (FnExpr param blk) = "fun (" ++ intercalate ", " (map show param) ++ ")" ++ show blk
    show Invalid = "InvalidExpression"



