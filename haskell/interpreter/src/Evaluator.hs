{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Evaluator (evalProgram) where

import Ast qualified
import Object (typeObject)
import Object qualified

evalProgram :: Ast.Program -> Object.Object
evalProgram (Ast.Program statements) =
    let
        inner acc [] = acc
        inner _ (c : cs) =
            case evalStatement c of
                Object.RetObj v -> v
                c@(Object.ErrObj _) -> c
                curr -> inner curr cs
     in
        inner Object.Null statements

evalStatement :: Ast.Statement -> Object.Object
evalStatement statement = case statement of
    Ast.LetStatement ident expr -> undefined
    Ast.ReturnStatement expr -> Object.RetObj $ evalExpression expr
    Ast.ExpressionStatement expr -> evalExpression expr
    Ast.BlockStatement statements -> evalBlockStatement statements

evalBlockStatement :: [Ast.Statement] -> Object.Object
evalBlockStatement statements =
    let
        inner acc [] = acc
        inner _ (c : cs) =
            case evalStatement c of
                curr@(Object.ErrObj _) -> curr
                curr@(Object.RetObj _) -> curr
                curr -> inner curr cs
     in
        inner Object.Null statements

evalExpression :: Ast.Expression -> Object.Object
evalExpression expr =
    let
        checkOps l r =
            let
                ls = typeObject l
                rs = typeObject r
                pref = if ls == rs then "unknown operator: " else "type mismatch: "
             in
                Object.ErrObj $ pref ++ ls ++ " + " ++ rs

        evalOne f v = f $ evalExpression v

        evalTwoInt p f l r = case (evalExpression l, evalExpression r) of
            (Object.IntObj l, Object.IntObj r) -> p $ f l r
            (l, r) -> checkOps l r

        evalEqExpr l r = case (evalExpression l, evalExpression r) of
            (Object.BoolObj l, Object.BoolObj r) -> Object.BoolObj $ l == r
            (Object.IntObj l, Object.IntObj r) -> Object.BoolObj $ l == r
            (l, r) -> checkOps l r

        evalNeqExpr l r = case (evalExpression l, evalExpression r) of
            (Object.BoolObj l, Object.BoolObj r) -> Object.BoolObj $ l /= r
            (Object.IntObj l, Object.IntObj r) -> Object.BoolObj $ l /= r
            (l, r) -> checkOps l r
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
            Ast.IfExpr cond cons alt -> evalIfExpr cond cons alt
            expr -> error . show $ expr

evalIfExpr ::
    Ast.Expression ->
    Ast.Statement ->
    Maybe Ast.Statement ->
    Object.Object
evalIfExpr cond cons alt =
    if evalTruthy . evalExpression $ cond
        then evalStatement cons
        else maybe Object.Null evalStatement alt

evalTruthy :: Object.Object -> Bool
evalTruthy o = case o of
    Object.BoolObj b -> b
    Object.Null -> False
    _ -> True

evalNegPrefix :: Object.Object -> Object.Object
evalNegPrefix v = case v of
    Object.BoolObj b -> Object.BoolObj (not b)
    Object.Null -> Object.BoolObj True
    _ -> Object.BoolObj False

evalMinusPrefix :: Object.Object -> Object.Object
evalMinusPrefix obj = case obj of
    (Object.IntObj v) -> Object.IntObj (negate v)
    c -> Object.ErrObj $ "unknown operator: -" ++ typeObject c
