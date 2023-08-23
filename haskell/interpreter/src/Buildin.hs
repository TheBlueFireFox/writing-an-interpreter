module Buildin (parentEnv) where

import Control.Arrow (Arrow (second))
import Environment qualified as Env
import Object qualified
import Text.Printf

parentEnv :: Object.Env
parentEnv =
    let
        lst =
            [ ("len", len)
            , ("first", first)
            , ("last", blast)
            , ("rest", rest)
            , ("push", push)
            ]
        inner env [] = env
        inner env ((name, obj) : cs) = inner (Env.setEnv env name obj) cs
     in
        inner Env.newEnv $ map (second Object.BuiObj) lst

len :: [Object.Object] -> Object.Object
len =
    let
        inner [arg] = case arg of
            Object.ArrObj s -> Object.IntObj $ fromIntegral $ length s
            Object.StrObj s -> Object.IntObj $ fromIntegral $ length s
            other -> Object.ErrObj $ printf "argument to \"len\" not supported, got %s" $ Object.typeObject other
        inner d = Object.ErrObj $ printf "wrong number of arguments. got=%d, want=1" $ length d
     in
        inner

first :: [Object.Object] -> Object.Object
first =
    let
        inner [arg] = case arg of
            Object.ArrObj s -> head s
            other -> Object.ErrObj $ printf "argument to \"first\" must be ARRAY, got %s" $ Object.typeObject other
        inner d = Object.ErrObj $ printf "wrong number of arguments. got=%d, want=1" $ length d
     in
        inner

blast :: [Object.Object] -> Object.Object
blast =
    let
        inner [arg] = case arg of
            Object.ArrObj s -> last s
            other -> Object.ErrObj $ printf "argument to \"last\" must be ARRAY, got %s" $ Object.typeObject other
        inner d = Object.ErrObj $ printf "wrong number of arguments. got=%d, want=1" $ length d
     in
        inner

rest :: [Object.Object] -> Object.Object
rest =
    let
        inner [arg] = case arg of
            Object.ArrObj (_ : xs) -> Object.ArrObj xs
            Object.ArrObj [] -> Object.Null
            other -> Object.ErrObj $ printf "argument to \"rest\" must be ARRAY, got %s" $ Object.typeObject other
        inner d = Object.ErrObj $ printf "wrong number of arguments. got=%d, want=1" $ length d
     in
        inner

push :: [Object.Object] -> Object.Object
push =
    let
        inner [arr, obj] = case arr of
            Object.ArrObj x -> Object.ArrObj (x ++ [obj])
            other -> Object.ErrObj $ printf "argument to \"push\" must be ARRAY, got %s" $ Object.typeObject other
        inner d = Object.ErrObj $ printf "wrong number of arguments. got=%d, want=2" $ length d
     in
        inner
