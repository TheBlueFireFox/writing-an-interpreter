{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Evaluator (evalProgram) where

import Ast qualified
import Debug.Trace (trace)
import Object qualified

evalProgram :: Ast.Program -> [Object.Object]
evalProgram (Ast.Program statements) = evalStatements statements

evalStatements :: [Ast.Statement] -> [Object.Object]
evalStatements =
    let
        inner acc [] = reverse acc
        inner acc (c : cs) = inner (evalStatement c : acc) cs
     in
        inner []

evalStatement :: Ast.Statement -> Object.Object
evalStatement statement = case statement of
    Ast.LetStatement ident expr -> undefined
    Ast.ReturnStatement expr -> undefined
    Ast.ExpressionStatement expr -> evalExpression expr
    Ast.BlockStatement statements -> undefined

evalExpression :: Ast.Expression -> Object.Object
evalExpression expr =
    let
        evalOne f v = f $ evalExpression v
        evalTwoInt p f l r = case (evalExpression l, evalExpression r) of
            (Object.IntObj l, Object.IntObj r) -> p $ f l r
            _ -> Object.Null

        evalEqExpr l r = case (evalExpression l, evalExpression r) of
            (Object.BoolObj l, Object.BoolObj r) -> Object.BoolObj $ l == r
            (Object.IntObj l, Object.IntObj r) -> Object.BoolObj $ l == r
            _ -> Object.Null

        evalNeqExpr l r = case (evalExpression l, evalExpression r) of
            (Object.BoolObj l, Object.BoolObj r) -> Object.BoolObj $ l /= r
            (Object.IntObj l, Object.IntObj r) -> Object.BoolObj $ l /= r
            _ -> Object.Null
     in
        case expr of
            Ast.IntegerExpr v -> Object.IntObj v
            Ast.BooleanExpr v -> Object.BoolObj v
            Ast.NotExpr v -> evalOne evalNegPrefix v
            Ast.NegExpr v -> evalOne evalMinusPrefix v
            Ast.AddExpr l r -> evalTwoInt Object.IntObj (+) l r
            Ast.MinExpr l r -> evalTwoInt Object.IntObj (-) l r
            Ast.MulExpr l r -> evalTwoInt Object.IntObj (*) l r
            Ast.DivExpr l r -> evalTwoInt Object.IntObj div l r
            Ast.LeExpr l r -> evalTwoInt Object.BoolObj (<) l r
            Ast.GtExpr l r -> evalTwoInt Object.BoolObj (>) l r
            Ast.EqExpr l r -> evalEqExpr l r
            Ast.NeqExpr l r -> evalNeqExpr l r
            expr -> error . show $ expr

evalNegPrefix :: Object.Object -> Object.Object
evalNegPrefix v = case v of
    Object.BoolObj b -> Object.BoolObj (not b)
    Object.Null -> Object.BoolObj True
    _ -> Object.BoolObj False

evalMinusPrefix :: Object.Object -> Object.Object
evalMinusPrefix (Object.IntObj v) = Object.IntObj (negate v)
evalMinusPrefix _ = Object.Null
