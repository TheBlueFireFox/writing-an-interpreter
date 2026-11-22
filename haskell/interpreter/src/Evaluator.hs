module Evaluator (evalProgram) where

import Ast qualified
import Control.Monad (foldM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (runExceptT)
import Data.Bifunctor (first)
import Data.HashMap.Strict (HashMap, fromList, (!?))
import Data.Int (Int64)
import Data.List (genericIndex)
import Data.Maybe (fromMaybe)
import Environment qualified as Env
import Object (Abandon, Result, failWith)
import Object qualified
import Text.Printf (printf)

evalProgram :: Object.Env -> Ast.Program -> IO (Either String (Object.Object, Object.Env))
evalProgram env prog = do
    res <- evalProgramHelper env prog
    case res of
        Left (Object.Error err) -> pure $ Left err
        Right ret -> pure $ Right ret

evalProgramHelper :: Object.Env -> Ast.Program -> IO (Either Abandon (Object.Object, Object.Env))
evalProgramHelper env (Ast.Program statements) = runExceptT $ inner env Object.Null statements
  where
    inner :: Object.Env -> Object.Object -> [Ast.Statement] -> Result (Object.Object, Object.Env)
    inner env' lst [] = pure (lst, env')
    inner env' _ (curr : cs) = do
        s <- evalStatement env' curr
        case s of
            (Object.RetObj v, env'') -> pure (v, env'')
            (next, env'') -> inner env'' next cs

evalStatement :: Object.Env -> Ast.Statement -> Result (Object.Object, Object.Env)
evalStatement env statement = case statement of
    Ast.LetStatement ident expr -> (Object.Null,) <$> evalLetStatement env ident expr
    Ast.ReturnStatement expr -> first Object.RetObj <$> inner evalExpression expr
    Ast.ExpressionStatement expr -> inner evalExpression expr
    Ast.BlockStatement statements -> inner evalBlockStatement statements
  where
    inner fn param = do
        r <- fn env param
        pure (r, env)

evalLetStatement :: Env.Env Object.Object -> String -> Ast.Expression -> Result (Object.Env)
evalLetStatement env ident expr = do
    res <- evalExpression env expr
    liftIO $ Env.setEnv env ident res

evalBlockStatement :: Object.Env -> [Ast.Statement] -> Result Object.Object
evalBlockStatement env = inner env Object.Null
  where
    inner _ acc [] = pure acc
    inner env' _ (c : cs) = do
        s <- evalStatement env' c
        case s of
            (curr@(Object.RetObj _), _) -> pure curr
            (curr, env'') -> inner env'' curr cs

evalExpression :: Object.Env -> Ast.Expression -> Result Object.Object
evalExpression env expr =
    case expr of
        Ast.IdentExpr v -> evalIdent env v
        Ast.IntegerExpr v -> pure $ Object.IntObj v
        Ast.BooleanExpr v -> pure $ Object.BoolObj v
        Ast.StrExpr v -> pure $ Object.StrObj v
        Ast.NotExpr v -> evalOne env evalNegPrefix v
        Ast.NegExpr v -> evalOneM env evalMinusPrefix v
        Ast.AddExpr l r -> evalAddExpr env l r
        Ast.MinExpr l r -> evalTwoInt env Object.IntObj (-) "-" l r
        Ast.MulExpr l r -> evalTwoInt env Object.IntObj (*) "*" l r
        Ast.DivExpr l r -> evalTwoIntM env evalTwoDiv "/" l r
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

evalIndexExpr :: Object.Env -> Ast.Expression -> Ast.Expression -> Result Object.Object
evalIndexExpr = evalTwoExpr fn
  where
    fn l' r' =
        case (l', r') of
            (Object.ArrObj objs, Object.IntObj v) -> pure $ evalArrayIndexExpr objs v
            (Object.HashObj objs, v) -> evalHashIndexExpr objs v
            (l, _) -> failWith $ "index operator not supported: " ++ Object.typeObject l

evalArrayIndexExpr :: (Integral a) => [Object.Object] -> a -> Object.Object
evalArrayIndexExpr objs index
    | index < 0 || length objs <= fromIntegral index = Object.Null
    | otherwise = genericIndex objs index

evalHashIndexExpr :: HashMap Object.HashableObject Object.Object -> Object.Object -> Result Object.Object
evalHashIndexExpr m key = case Object.toHashKey key of
    Left err -> failWith err
    Right hashableKey -> pure $ fromMaybe Object.Null $ m !? hashableKey

checkOps :: Object.Object -> Object.Object -> [Char] -> Result Object.Object
checkOps l r s = failWith $ pref ++ ls ++ " " ++ s ++ " " ++ rs
  where
    ls = Object.typeObject l
    rs = Object.typeObject r
    pref
        | ls == rs = "unknown operator: "
        | otherwise = "type mismatch: "

evalTwoExpr ::
    (Object.Object -> Object.Object -> Result b) ->
    Object.Env ->
    Ast.Expression ->
    Ast.Expression ->
    Result b
evalTwoExpr fn env left right = do
    l' <- evalExpression env left
    r' <- evalExpression env right
    fn l' r'

evalOneM ::
    Object.Env ->
    (Object.Object -> Result b) ->
    Ast.Expression ->
    Result b
evalOneM env fun val = fun =<< evalExpression env val

evalOne :: Object.Env -> (Object.Object -> b) -> Ast.Expression -> Result b
evalOne env fun val = evalOneM env (pure . fun) val

evalTwoDiv :: Int64 -> Int64 -> Result Object.Object
evalTwoDiv l r
    | r == 0 = failWith "division by zero"
    | l == minBound && r == (-1) = failWith "arithmetic overflow"
    | otherwise = pure $ Object.IntObj $ quot l r

evalTwoIntM ::
    Object.Env ->
    (Int64 -> Int64 -> Result Object.Object) ->
    [Char] ->
    Ast.Expression ->
    Ast.Expression ->
    Result Object.Object
evalTwoIntM env fun sFun = evalTwoExpr fn env
  where
    fn l' r' =
        case (l', r') of
            (Object.IntObj l, Object.IntObj r) -> fun l r
            (l, r) -> checkOps l r sFun

evalTwoInt ::
    Object.Env ->
    (a -> Object.Object) ->
    (Int64 -> Int64 -> a) ->
    [Char] ->
    Ast.Expression ->
    Ast.Expression ->
    Result Object.Object
evalTwoInt env toObj fun = evalTwoIntM env (\l r -> pure $ toObj $ fun l r)

evalEqExpr :: Object.Env -> Ast.Expression -> Ast.Expression -> Result Object.Object
evalEqExpr = evalTwoExpr fn
  where
    fn l' r' =
        case (l', r') of
            (Object.BoolObj l, Object.BoolObj r) -> pure $ Object.BoolObj $ l == r
            (Object.IntObj l, Object.IntObj r) -> pure $ Object.BoolObj $ l == r
            (l, r) -> checkOps l r "=="

evalAddExpr :: Object.Env -> Ast.Expression -> Ast.Expression -> Result Object.Object
evalAddExpr = evalTwoExpr fn
  where
    fn l' r' =
        case (l', r') of
            (Object.IntObj l, Object.IntObj r) -> pure $ Object.IntObj $ l + r
            (Object.StrObj l, Object.StrObj r) -> pure $ Object.StrObj $ l ++ r
            (l, r) -> checkOps l r "+"

evalNeqExpr :: Object.Env -> Ast.Expression -> Ast.Expression -> Result Object.Object
evalNeqExpr = evalTwoExpr fn
  where
    fn l' r' =
        case (l', r') of
            (Object.BoolObj l, Object.BoolObj r) -> pure $ Object.BoolObj $ l /= r
            (Object.IntObj l, Object.IntObj r) -> pure $ Object.BoolObj $ l /= r
            (l, r) -> checkOps l r "!="

evalIdent :: Object.Env -> String -> Result Object.Object
evalIdent env name = do
    res <- liftIO (Env.getEnv env name)
    case res of
        Just has -> pure has
        Nothing -> failWith $ "identifier not found: " ++ name

evalIfExpr ::
    Object.Env ->
    Ast.Expression ->
    Ast.Statement ->
    Maybe Ast.Statement ->
    Result Object.Object
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

evalMinusPrefix :: Object.Object -> Result Object.Object
evalMinusPrefix obj = case obj of
    (Object.IntObj v) -> pure $ Object.IntObj (negate v)
    c -> failWith $ "unknown operator: -" ++ Object.typeObject c

evalCallExpr :: Ast.Expression -> [Ast.Expression] -> Object.Env -> Result Object.Object
evalCallExpr fn args env = do
    l' <- evalExpression env fn
    r' <- evalExpressions env args
    case (l', r') of
        (Object.FnObj params body fnEnv, args') -> applyFn params body fnEnv args'
        (Object.BuiObj fn', args') -> fn' args'
        (l, _) -> failWith $ "not a function: " ++ Object.typeObject l

evalExpressions :: Env.Env Object.Object -> [Ast.Expression] -> Result [Object.Object]
evalExpressions env = mapM (evalExpression env)

applyFn :: [Ast.Expression] -> Ast.Statement -> Object.Env -> [Object.Object] -> Result Object.Object
applyFn params body fnEnv args
    | lenParams /= lenArgs = failWith $ printf "wrong number of arguments. got=%d, want=%d" lenArgs lenParams
    | otherwise = do
        efn <- extendFnEnv params fnEnv args
        b <- evalStatement efn body
        pure $ case b of
            (Object.RetObj v, _) -> v
            (other, _) -> other
  where
    lenParams = length params
    lenArgs = length args

extendFnEnv :: [Ast.Expression] -> Env.Env a -> [a] -> Result (Env.Env a)
extendFnEnv params fnEnv args = do
    fenv <- liftIO $ Env.newEnclosedEnv fnEnv
    -- zip is okay here as we ensure on applyFn that params and args are of equal length
    foldM apply fenv (zip params args)
  where
    unwrapExpr val = case val of
        (Ast.IdentExpr ident) -> ident
        v -> error $ "unexpected parameter expression: " ++ show v

    apply env (param, arg) = liftIO $ Env.setEnv env (unwrapExpr param) arg

evalHashExpr :: Object.Env -> [(Ast.Expression, Ast.Expression)] -> Result Object.Object
evalHashExpr env = fmap (Object.HashObj . fromList) . traverse handleExpr
  where
    handleExpr (k, v) = do
        k' <- evalExpression env k
        keyHash <- either failWith pure $ Object.toHashKey k'
        value <- evalExpression env v
        pure (keyHash, value)
