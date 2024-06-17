module Evaluator (evalProgram) where

import Ast (Display (dprint))
import Ast qualified
import Buildin qualified
import Control.Applicative ((<|>))
import Control.Arrow (first)
import Control.Monad (foldM, liftM2)
import Data.Either (partitionEithers)
import Data.HashMap.Strict (HashMap, fromList, (!?))
import Data.Int (Int64)
import Data.List (genericIndex)
import Data.Maybe (fromMaybe)
import Environment qualified as Env
import Object qualified

evalProgram :: Object.Env -> Ast.Program -> IO (Object.Object, Object.Env)
evalProgram env (Ast.Program statements) = inner env Object.Null statements
  where
    inner :: Object.Env -> Object.Object -> [Ast.Statement] -> IO (Object.Object, Object.Env)
    inner env' lst [] = pure (lst, env')
    inner env' _ (curr : cs) = do
        s <- evalStatement env' curr
        case s of
            (Object.RetObj v, env'') -> pure (v, env'')
            (c@(Object.ErrObj _), env'') -> pure (c, env'')
            (next, env'') -> inner env'' next cs

evalStatement :: Object.Env -> Ast.Statement -> IO (Object.Object, Object.Env)
evalStatement env statement = case statement of
    Ast.LetStatement ident expr -> (Object.Null,) <$> evalLetStatement env ident expr
    Ast.ReturnStatement expr -> first Object.RetObj <$> inner evalExpression expr
    Ast.ExpressionStatement expr -> inner evalExpression expr
    Ast.BlockStatement statements -> inner evalBlockStatement statements
  where
    inner fn param = do
        r <- fn env param
        pure (r, env)

evalLetStatement :: Env.Env Object.Object -> String -> Ast.Expression -> IO (Env.Env Object.Object)
evalLetStatement env ident expr = Env.setEnv env ident =<< evalExpression env expr

evalBlockStatement :: Object.Env -> [Ast.Statement] -> IO Object.Object
evalBlockStatement env = inner env Object.Null
  where
    inner _ acc [] = pure acc
    inner env' _ (c : cs) = do
        s <- evalStatement env' c
        case s of
            (curr@(Object.ErrObj _), _) -> pure curr
            (curr@(Object.RetObj _), _) -> pure curr
            (curr, env'') -> inner env'' curr cs

evalExpression :: Object.Env -> Ast.Expression -> IO Object.Object
evalExpression env expr =
    case expr of
        Ast.IdentExpr v -> evalIdent env v
        Ast.IntegerExpr v -> pure $ Object.IntObj v
        Ast.BooleanExpr v -> pure $ Object.BoolObj v
        Ast.StrExpr v -> pure $ Object.StrObj v
        Ast.NotExpr v -> evalOne env evalNegPrefix v
        Ast.NegExpr v -> evalOne env evalMinusPrefix v
        Ast.AddExpr l r -> evalAddExpr env l r
        Ast.MinExpr l r -> evalTwoInt env Object.IntObj (-) "-" l r
        Ast.MulExpr l r -> evalTwoInt env Object.IntObj (*) "*" l r
        Ast.DivExpr l r -> evalTwoInt env Object.IntObj div "/" l r
        Ast.LeExpr l r -> evalTwoInt env Object.BoolObj (<) "<" l r
        Ast.GtExpr l r -> evalTwoInt env Object.BoolObj (>) ">" l r
        Ast.EqExpr l r -> evalEqExpr env l r
        Ast.NeqExpr l r -> evalNeqExpr env l r
        Ast.IfExpr cond cons alt -> evalIfExpr env cond cons alt
        Ast.FnExpr params blk -> pure $ Object.FnObj params blk env
        Ast.CallExpr fn args -> evalCallExpr fn args env
        Ast.ArrExpr objs -> Object.ArrObj <$> evalExpressions env objs
        Ast.IndExpr left index -> evalIndexExpr env left index
        Ast.HashExpr m -> evalHashExpr env m

evalIndexExpr :: Object.Env -> Ast.Expression -> Ast.Expression -> IO Object.Object
evalIndexExpr = evalTwoExpr fn
  where
    fn l' r' =
        pure $ case (l', r') of
            (err@(Object.ErrObj _), _) -> err
            (_, err@(Object.ErrObj _)) -> err
            (Object.ArrObj objs, Object.IntObj v) -> evalArrayIndexExpr objs v
            (Object.HashObj objs, v) -> evalHashIndexExpr objs v
            (l, _) -> Object.ErrObj $ "index operator not supported: " ++ Object.typeObject l

evalArrayIndexExpr :: (Integral a) => [Object.Object] -> a -> Object.Object
evalArrayIndexExpr objs index
    | index < 0 || length objs <= fromIntegral index = Object.Null
    | otherwise = genericIndex objs index

evalHashIndexExpr :: HashMap Object.Object Object.Object -> Object.Object -> Object.Object
evalHashIndexExpr m key = case Object.genSimpleHash key of
    Right _ -> fromMaybe Object.Null $ m !? key
    Left err -> Object.ErrObj err

checkOps :: Object.Object -> Object.Object -> [Char] -> Object.Object
checkOps l r s = Object.ErrObj $ pref ++ ls ++ " " ++ s ++ " " ++ rs
  where
    ls = Object.typeObject l
    rs = Object.typeObject r
    pref
        | ls == rs = "unknown operator: "
        | otherwise = "type mismatch: "

evalTwoExpr :: (Object.Object -> Object.Object -> IO b) -> Object.Env -> Ast.Expression -> Ast.Expression -> IO b
evalTwoExpr fn env left right = do
    l' <- evalExpression env left
    r' <- evalExpression env right
    fn l' r'

evalOne :: Object.Env -> (Object.Object -> b) -> Ast.Expression -> IO b
evalOne env fun val = fun <$> evalExpression env val

evalTwoInt ::
    Object.Env ->
    (a -> Object.Object) ->
    (Int64 -> Int64 -> a) ->
    String ->
    Ast.Expression ->
    Ast.Expression ->
    IO Object.Object
evalTwoInt env toObj fun sFun = evalTwoExpr fn env
  where
    fn l' r' =
        pure $ case (l', r') of
            (Object.IntObj l, Object.IntObj r) -> toObj $ fun l r
            (l, r) -> checkOps l r sFun

evalEqExpr :: Object.Env -> Ast.Expression -> Ast.Expression -> IO Object.Object
evalEqExpr = evalTwoExpr fn
  where
    fn l' r' =
        pure $ case (l', r') of
            (Object.BoolObj l, Object.BoolObj r) -> Object.BoolObj $ l == r
            (Object.IntObj l, Object.IntObj r) -> Object.BoolObj $ l == r
            (l, r) -> checkOps l r "=="

evalAddExpr :: Object.Env -> Ast.Expression -> Ast.Expression -> IO Object.Object
evalAddExpr = evalTwoExpr fn
  where
    fn l' r' =
        pure $ case (l', r') of
            (Object.IntObj l, Object.IntObj r) -> Object.IntObj $ l + r
            (Object.StrObj l, Object.StrObj r) -> Object.StrObj $ l ++ r
            (l, r) -> checkOps l r "+"

evalNeqExpr :: Object.Env -> Ast.Expression -> Ast.Expression -> IO Object.Object
evalNeqExpr = evalTwoExpr fn
  where
    fn l' r' =
        pure $ case (l', r') of
            (Object.BoolObj l, Object.BoolObj r) -> Object.BoolObj $ l /= r
            (Object.IntObj l, Object.IntObj r) -> Object.BoolObj $ l /= r
            (l, r) -> checkOps l r "!="

evalIdent :: Object.Env -> String -> IO Object.Object
evalIdent env name = fromMaybe fallback <$> get
  where
    fallback = Object.ErrObj $ "identifier not found: " ++ name
    get = liftM2 (<|>) (Env.getEnv env name) ((`Env.getEnv` name) =<< Buildin.parentEnv)

evalIfExpr ::
    Object.Env ->
    Ast.Expression ->
    Ast.Statement ->
    Maybe Ast.Statement ->
    IO Object.Object
evalIfExpr env cond cons alt = handleIf =<< evalExpression env cond
  where
    handleIf c
        | evalTruthy c = fst <$> evalStatement env cons
        | otherwise = handleAlt alt
    handleAlt Nothing = pure Object.Null
    handleAlt (Just v) = fst <$> evalStatement env v

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

evalCallExpr :: Ast.Expression -> [Ast.Expression] -> Object.Env -> IO Object.Object
evalCallExpr fn args env = do
    l' <- evalExpression env fn
    r' <- evalExpressions env args
    case (l', r') of
        (err@(Object.ErrObj _), _) -> pure err
        (_, [err@(Object.ErrObj _)]) -> pure err
        (Object.FnObj params body fnEnv, args') -> applyFn params body fnEnv args'
        (Object.BuiObj fn', args') -> fn' args'
        (l, _) -> error $ "not a function: " ++ dprint l

evalExpressions :: Env.Env Object.Object -> [Ast.Expression] -> IO [Object.Object]
evalExpressions env = inner []
  where
    inner acc [] = pure $ reverse acc
    inner acc (c : cs) = check acc cs =<< evalExpression env c

    check acc cs f = case f of
        err@(Object.ErrObj _) -> pure [err]
        obj -> inner (obj : acc) cs

applyFn :: [Ast.Expression] -> Ast.Statement -> Object.Env -> [Object.Object] -> IO Object.Object
applyFn params body fnEnv args = do
    efn <- extendFnEnv params fnEnv args
    b <- evalStatement efn body
    pure $ case b of
        (Object.RetObj v, _) -> v
        (other, _) -> other

extendFnEnv :: [Ast.Expression] -> Env.Env a -> [a] -> IO (Env.Env a)
extendFnEnv params fnEnv args = do
    fenv <- Env.newEnclosedEnv fnEnv
    foldM apply fenv (zip params args)
  where
    unwrapExpr val = case val of
        (Ast.IdentExpr ident) -> ident
        v -> error $ "unexpected parameter expression: " ++ show v

    apply env (param, arg) = Env.setEnv env (unwrapExpr param) arg

evalHashExpr :: Object.Env -> [(Ast.Expression, Ast.Expression)] -> IO Object.Object
evalHashExpr env m = handleError . partitionEithers <$> mapM handleExpr m
  where
    handleExpr (k, v) = do
        k' <- evalExpression env k
        v' <- evalExpression env v
        pure $ uncurry genHash =<< handler k' v'

    handler k v = case (k, v) of
        (err@(Object.ErrObj _), _) -> Left err
        (_, err@(Object.ErrObj _)) -> Left err
        _ -> Right (k, v)

    genHash key value = case Object.genSimpleHash key of
        Right _ -> Right (key, value)
        Left err -> Left $ Object.ErrObj err

    handleError (l, r)
        | null l = Object.HashObj $ fromList r
        | otherwise = head l
