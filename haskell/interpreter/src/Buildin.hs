module Buildin (parentEnv) where

import Control.Arrow (Arrow (second))
import Environment qualified as Env
import Object qualified
import Text.Printf

parentEnv :: IO Object.Env
parentEnv = do
    empty <- Env.newEnv
    inner empty $ map (second Object.BuiObj) lst
  where
    lst =
        [ ("len", len)
        , ("first", first)
        , ("last", blast)
        , ("rest", rest)
        , ("push", push)
        ]
    inner env [] = pure env
    inner env ((name, obj) : cs) = do
        env' <- Env.setEnv env name obj
        inner env' cs

len :: [Object.Object] -> Object.Object
len = inner
  where
    inner [arg] = case arg of
        Object.ArrObj s -> Object.IntObj $ fromIntegral $ length s
        Object.StrObj s -> Object.IntObj $ fromIntegral $ length s
        other -> Object.ErrObj $ printf "argument to \"len\" not supported, got %s" $ Object.typeObject other
    inner d = Object.ErrObj $ printf "wrong number of arguments. got=%d, want=1" $ length d

first :: [Object.Object] -> Object.Object
first = inner
  where
    inner [arg] = case arg of
        Object.ArrObj s -> head s
        other -> Object.ErrObj $ printf "argument to \"first\" must be ARRAY, got %s" $ Object.typeObject other
    inner d = Object.ErrObj $ printf "wrong number of arguments. got=%d, want=1" $ length d

blast :: [Object.Object] -> Object.Object
blast = inner
  where
    inner [arg] = case arg of
        Object.ArrObj s -> last s
        other -> Object.ErrObj $ printf "argument to \"last\" must be ARRAY, got %s" $ Object.typeObject other
    inner d = Object.ErrObj $ printf "wrong number of arguments. got=%d, want=1" $ length d

rest :: [Object.Object] -> Object.Object
rest = inner
  where
    inner [arg] = case arg of
        Object.ArrObj (_ : xs) -> Object.ArrObj xs
        Object.ArrObj [] -> Object.Null
        other -> Object.ErrObj $ printf "argument to \"rest\" must be ARRAY, got %s" $ Object.typeObject other
    inner d = Object.ErrObj $ printf "wrong number of arguments. got=%d, want=1" $ length d

push :: [Object.Object] -> Object.Object
push = inner
  where
    inner [arr, obj] = case arr of
        Object.ArrObj x -> Object.ArrObj (x ++ [obj])
        other -> Object.ErrObj $ printf "argument to \"push\" must be ARRAY, got %s" $ Object.typeObject other
    inner d = Object.ErrObj $ printf "wrong number of arguments. got=%d, want=2" $ length d
