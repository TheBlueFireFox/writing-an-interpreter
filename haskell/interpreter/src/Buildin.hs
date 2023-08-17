module Buildin (parentEnv) where

import Environment qualified as Env
import Object (BuildInFunction)
import Object qualified
import Text.Printf

parentEnv :: Object.Env
parentEnv =
    let
        lst =
            [ ("len", Object.BuiObj len)
            ]
        inner env [] = env
        inner env ((name, obj) : cs) = inner (Env.setEnv env name obj) cs
     in
        inner Env.newEnv lst

len :: BuildInFunction
len =
    let
        inner [arg] = case arg of
            Object.StrObj s -> Object.IntObj $ fromIntegral $ length s
            other -> Object.ErrObj $ printf "argument to \"len\" not supported, got %s" $ Object.typeObject other
        inner d = Object.ErrObj $ printf "wrong number of arguments. got=%d, want=1" $ length d
     in
        Object.BuildInFunction inner
