{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module Ast (Program (..), Statement (..), Expression (..), Display (..)) where

import Data.Hashable (Hashable)
import Data.Int (Int64)
import Data.List (intercalate)
import GHC.Generics (Generic)

class Display a where
    dprint :: a -> String

newtype Program = Program [Statement]
    deriving (Eq, Show)

instance Display Program where
    dprint (Program statements) = concatMap dprint statements

data Statement
    = LetStatement String Expression
    | ReturnStatement Expression
    | ExpressionStatement Expression
    | BlockStatement [Statement]
    deriving stock (Eq, Show, Generic)
    deriving anyclass (Hashable)

instance Display Statement where
    dprint (LetStatement name expr) = "let " ++ name ++ " = " ++ dprint expr ++ ";"
    dprint (ReturnStatement expr) = "return " ++ dprint expr ++ ";"
    dprint (ExpressionStatement expr) = dprint expr
    dprint (BlockStatement exprs) = concatMap dprint exprs

data Expression
    = IdentExpr String
    | IntegerExpr Int64
    | BooleanExpr Bool
    | StrExpr String
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
    | IndExpr Expression Expression -- Object Index
    | ArrExpr [Expression] -- Elements
    | HashExpr [(Expression, Expression)] -- Elements
    | IfExpr Expression Statement (Maybe Statement) -- Condition BlockStatement BlockStatement
    | FnExpr [Expression] Statement -- Params BlockStatement
    | CallExpr Expression [Expression] -- Function Arguments
    deriving stock (Eq, Show, Generic)
    deriving anyclass (Hashable)

showHelperSingle :: (Display a) => String -> a -> String
showHelperSingle sym expr = "(" ++ sym ++ dprint expr ++ ")"

showHelperTwo :: (Display a1, Display a2) => String -> a1 -> a2 -> String
showHelperTwo sym l r = "(" ++ dprint l ++ " " ++ sym ++ " " ++ dprint r ++ ")"

showHelperIf ::
    (Display a1, Display a2, Display a3) =>
    a1 ->
    a2 ->
    Maybe a3 ->
    String
showHelperIf cond cons alt = "if " ++ dprint cond ++ " " ++ dprint cons ++ maybe "" (\x -> "else " ++ dprint x) alt

instance Display Expression where
    dprint (IdentExpr lit) = lit
    dprint (IntegerExpr lit) = show lit
    dprint (BooleanExpr True) = "true"
    dprint (BooleanExpr False) = "false"
    dprint (StrExpr lit) = lit
    dprint (NegExpr expr) = showHelperSingle "-" expr
    dprint (NotExpr expr) = showHelperSingle "!" expr
    dprint (AddExpr l r) = showHelperTwo "+" l r
    dprint (MinExpr l r) = showHelperTwo "-" l r
    dprint (MulExpr l r) = showHelperTwo "*" l r
    dprint (DivExpr l r) = showHelperTwo "/" l r
    dprint (NeqExpr l r) = showHelperTwo "!=" l r
    dprint (EqExpr l r) = showHelperTwo "==" l r
    dprint (GtExpr l r) = showHelperTwo ">" l r
    dprint (LeExpr l r) = showHelperTwo "<" l r
    dprint (IndExpr obj ind) = "(" ++ dprint obj ++ "[" ++ dprint ind ++ "])"
    dprint (ArrExpr arr) = "[" ++ intercalate ", " (map dprint arr) ++ "]"
    dprint (HashExpr m) = "{" ++ intercalate ", " (map (\(l, r) -> dprint l ++ ": " ++ dprint r) m) ++ "}"
    dprint (IfExpr cond cons alt) = showHelperIf cond cons alt
    dprint (FnExpr param blk) = "fun (" ++ intercalate ", " (map dprint param) ++ ")" ++ dprint blk
    dprint (CallExpr fn param) = dprint fn ++ "(" ++ intercalate ", " (map dprint param) ++ ")"
