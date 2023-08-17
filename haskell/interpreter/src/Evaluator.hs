module Evaluator (evalProgram) where

import Ast qualified
import Data.Int (Int64)
import Data.Maybe (fromMaybe)
import Environment qualified as Env
import Object qualified

evalProgram :: Object.Env -> Ast.Program -> (Object.Object, Object.Env)
evalProgram env (Ast.Program statements) =
    let
        inner :: Object.Env -> Object.Object -> [Ast.Statement] -> (Object.Object, Object.Env)
        inner env' lst [] = (lst, env')
        inner env' _ (curr : cs) =
            case evalStatement env' curr of
                (Object.RetObj v, env'') -> (v, env'')
                (c@(Object.ErrObj _), env'') -> (c, env'')
                (next, env'') -> inner env'' next cs
     in
        inner env Object.Null statements

evalStatement :: Object.Env -> Ast.Statement -> (Object.Object, Object.Env)
evalStatement env statement = case statement of
    Ast.LetStatement ident expr -> (Object.Null, Env.setEnv env ident $ evalExpression env expr)
    Ast.ReturnStatement expr -> (Object.RetObj $ evalExpression env expr, env)
    Ast.ExpressionStatement expr -> (evalExpression env expr, env)
    Ast.BlockStatement statements -> (evalBlockStatement env statements, env)

evalBlockStatement :: Object.Env -> [Ast.Statement] -> Object.Object
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

evalExpression :: Object.Env -> Ast.Expression -> Object.Object
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
        Ast.FnExpr params blk -> Object.FnObj params blk env
        Ast.CallExpr fn args -> evalCallExpr fn args env

evalCallExpr :: Ast.Expression -> [Ast.Expression] -> Object.Env -> Object.Object
evalCallExpr fn args env = case (evalExpression env fn, evalCallArgs env args) of
    (fn'@(Object.ErrObj _), _) -> fn'
    (_, [err@(Object.ErrObj _)]) -> err
    (Object.FnObj params body fnEnv, args') -> applyFn params body fnEnv args'
    t -> error $ "not a function: " ++ show t

evalCallArgs :: Env.Env Object.Object -> [Ast.Expression] -> [Object.Object]
evalCallArgs env args =
    let
        inner acc [] = reverse acc
        inner acc (c : cs) = case evalExpression env c of
            err@(Object.ErrObj _) -> [err]
            obj -> inner (obj : acc) cs
     in
        inner [] args

applyFn :: [Ast.Expression] -> Ast.Statement -> Object.Env -> [Object.Object] -> Object.Object
applyFn params body fnEnv args = case evalStatement (extendFnEnv params fnEnv args) body of
    (Object.RetObj v, _) -> v
    (other, _) -> other

extendFnEnv :: [Ast.Expression] -> Env.Env a -> [a] -> Env.Env a
extendFnEnv params fnEnv args =
    let
        unwrapExpr val = case val of
            (Ast.IdentExpr ident) -> ident
            v -> error $ "unexpected parameter expression: " ++ show v

        apply env (param, arg) = Env.setEnv env (unwrapExpr param) arg
     in
        foldl apply (Env.newEnclosedEnv fnEnv) (zip params args)

checkOps :: Object.Object -> Object.Object -> Object.Object
checkOps l r =
    let
        ls = Object.typeObject l
        rs = Object.typeObject r
        pref = if ls == rs then "unknown operator: " else "type mismatch: "
     in
        Object.ErrObj $ pref ++ ls ++ " + " ++ rs

evalOne :: Object.Env -> (Object.Object -> b) -> Ast.Expression -> b
evalOne env fun val = fun $ evalExpression env val

evalTwoInt ::
    Object.Env ->
    (a -> Object.Object) ->
    (Int64 -> Int64 -> a) ->
    Ast.Expression ->
    Ast.Expression ->
    Object.Object
evalTwoInt env toObj fun left right = case (evalExpression env left, evalExpression env right) of
    (Object.IntObj l, Object.IntObj r) -> toObj $ fun l r
    (l, r) -> checkOps l r

evalEqExpr :: Object.Env -> Ast.Expression -> Ast.Expression -> Object.Object
evalEqExpr env left right = case (evalExpression env left, evalExpression env right) of
    (Object.BoolObj l, Object.BoolObj r) -> Object.BoolObj $ l == r
    (Object.IntObj l, Object.IntObj r) -> Object.BoolObj $ l == r
    (l, r) -> checkOps l r

evalNeqExpr :: Object.Env -> Ast.Expression -> Ast.Expression -> Object.Object
evalNeqExpr env left right = case (evalExpression env left, evalExpression env right) of
    (Object.BoolObj l, Object.BoolObj r) -> Object.BoolObj $ l /= r
    (Object.IntObj l, Object.IntObj r) -> Object.BoolObj $ l /= r
    (l, r) -> checkOps l r

evalIdent :: Object.Env -> String -> Object.Object
evalIdent env name =
    let
        fallback = Object.ErrObj $ "identifier not found: " ++ name
     in
        fromMaybe fallback $ Env.getEnv env name

evalIfExpr ::
    Object.Env ->
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
