module Evaluator (evalProgram) where

import Ast qualified
import Data.Maybe (fromMaybe)
import Environment qualified as Env
import GHC.Int qualified
import Object qualified

evalProgram :: Env.Env -> Ast.Program -> (Object.Object, Env.Env)
evalProgram env (Ast.Program statements) =
    let
        inner :: Env.Env -> Object.Object -> [Ast.Statement] -> (Object.Object, Env.Env)
        inner env' lst [] = (lst, env')
        inner env' _ (curr : cs) =
            case evalStatement env' curr of
                (Object.RetObj v, env'') -> (v, env'')
                (c@(Object.ErrObj _), env'') -> (c, env'')
                (next, env'') -> inner env'' next cs
     in
        inner env Object.Null statements

evalStatement :: Env.Env -> Ast.Statement -> (Object.Object, Env.Env)
evalStatement env statement = case statement of
    Ast.LetStatement ident expr -> (Object.Null, Env.setEnv env ident $ evalExpression env expr)
    Ast.ReturnStatement expr -> (Object.RetObj $ evalExpression env expr, env)
    Ast.ExpressionStatement expr -> (evalExpression env expr, env)
    Ast.BlockStatement statements -> (evalBlockStatement env statements, env)

evalBlockStatement :: Env.Env -> [Ast.Statement] -> Object.Object
evalBlockStatement env statements =
    let
        inner _ acc [] = acc
        inner env' _ (c : cs) =
            case evalStatement env' c of
                (curr@(Object.ErrObj _), _) -> curr
                (curr@(Object.RetObj _), _) -> curr
                (curr, env'') -> inner env'' curr cs
     in
        inner env Object.Null statements

evalExpression :: Env.Env -> Ast.Expression -> Object.Object
evalExpression env expr =
    case expr of
        Ast.IdentExpr v -> evalIdent env v
        Ast.IntegerExpr v -> Object.IntObj v
        Ast.BooleanExpr v -> Object.BoolObj v
        Ast.NotExpr v -> evalOne env evalNegPrefix v
        Ast.NegExpr v -> evalOne env evalMinusPrefix v
        Ast.AddExpr l r -> evalTwoInt env Object.IntObj (+) l r
        Ast.MinExpr l r -> evalTwoInt env Object.IntObj (-) l r
        Ast.MulExpr l r -> evalTwoInt env Object.IntObj (*) l r
        Ast.DivExpr l r -> evalTwoInt env Object.IntObj div l r
        Ast.LeExpr l r -> evalTwoInt env Object.BoolObj (<) l r
        Ast.GtExpr l r -> evalTwoInt env Object.BoolObj (>) l r
        Ast.EqExpr l r -> evalEqExpr env l r
        Ast.NeqExpr l r -> evalNeqExpr env l r
        Ast.IfExpr cond cons alt -> evalIfExpr env cond cons alt
        expr' -> error . show $ expr'

checkOps :: Object.Object -> Object.Object -> Object.Object
checkOps l r =
    let
        ls = Object.typeObject l
        rs = Object.typeObject r
        pref = if ls == rs then "unknown operator: " else "type mismatch: "
     in
        Object.ErrObj $ pref ++ ls ++ " + " ++ rs

evalOne :: Env.Env -> (Object.Object -> b) -> Ast.Expression -> b
evalOne env fun val = fun $ evalExpression env val

evalTwoInt ::
    Env.Env ->
    (a -> Object.Object) ->
    (GHC.Int.Int64 -> GHC.Int.Int64 -> a) ->
    Ast.Expression ->
    Ast.Expression ->
    Object.Object
evalTwoInt env toObj fun left right = case (evalExpression env left, evalExpression env right) of
    (Object.IntObj l, Object.IntObj r) -> toObj $ fun l r
    (l, r) -> checkOps l r

evalEqExpr :: Env.Env -> Ast.Expression -> Ast.Expression -> Object.Object
evalEqExpr env left right = case (evalExpression env left, evalExpression env right) of
    (Object.BoolObj l, Object.BoolObj r) -> Object.BoolObj $ l == r
    (Object.IntObj l, Object.IntObj r) -> Object.BoolObj $ l == r
    (l, r) -> checkOps l r

evalNeqExpr :: Env.Env -> Ast.Expression -> Ast.Expression -> Object.Object
evalNeqExpr env left right = case (evalExpression env left, evalExpression env right) of
    (Object.BoolObj l, Object.BoolObj r) -> Object.BoolObj $ l /= r
    (Object.IntObj l, Object.IntObj r) -> Object.BoolObj $ l /= r
    (l, r) -> checkOps l r

evalIdent :: Env.Env -> String -> Object.Object
evalIdent env name =
    let
        fallback = Object.ErrObj $ "identifier not found: " ++ name
     in
        fromMaybe fallback $ Env.getEnv env name

evalIfExpr ::
    Env.Env ->
    Ast.Expression ->
    Ast.Statement ->
    Maybe Ast.Statement ->
    Object.Object
evalIfExpr env cond cons alt =
    if evalTruthy . evalExpression env $ cond
        then fst $ evalStatement env cons
        else maybe Object.Null (fst . evalStatement env) alt

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
    c -> Object.ErrObj $ "unknown operator: -" ++ Object.typeObject c
