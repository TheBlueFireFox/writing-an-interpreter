module Builtin (globalEnvironment) where

import Ast (dprint)
import Control.Monad (foldM)
import Control.Monad.IO.Class (liftIO)
import Data.List (uncons, unsnoc)
import Environment qualified as Env
import Object (Result)
import Object qualified
import Text.Printf

globalEnvironment :: IO Object.Env
globalEnvironment = Env.newEnclosedEnv =<< parentEnv

parentEnv :: IO Object.Env
parentEnv = do
    env <- Env.newEnv
    foldM inner env lst
  where
    lst =
        [ ("len", len)
        , ("first", first)
        , ("last", blast)
        , ("rest", rest)
        , ("push", push)
        , ("puts", puts)
        ]
    inner env (name, fn) = Env.setEnv env name (Object.BuiObj fn)

len :: [Object.Object] -> Result Object.Object
len obj
    | [Object.ArrObj s] <- obj = pure $ genInt s
    | [Object.StrObj s] <- obj = pure $ genInt s
    | [other] <- obj = Object.failWith $ printf "argument to \"len\" not supported, got %s" $ Object.typeObject other
    | otherwise = Object.failWith $ printf "wrong number of arguments. got=%d, want=1" $ length obj
  where
    genInt = Object.IntObj . fromIntegral . length

first :: [Object.Object] -> Result Object.Object
first = arrayHelper "first" inner
  where
    inner = maybe arrayEmptyErr (pure . fst) . uncons

blast :: [Object.Object] -> Result Object.Object
blast = arrayHelper "last" inner
  where
    inner = maybe arrayEmptyErr (pure . snd) . unsnoc

rest :: [Object.Object] -> Result Object.Object
rest = arrayHelper "rest" inner
  where
    inner = maybe arrayEmptyErr (pure . Object.ArrObj . snd) . uncons

push :: [Object.Object] -> Result Object.Object
push arr
    | [Object.ArrObj x, obj] <- arr = pure $ Object.ArrObj (x ++ [obj])
    | [other, _] <- arr = Object.failWith $ printf "first argument to \"push\" must be ARRAY, got %s" $ Object.typeObject other
    | otherwise = Object.failWith $ printf "wrong number of arguments. got=%d, want=2" $ length arr

arrayEmptyErr :: Result a
arrayEmptyErr = Object.failWith "ARRAY object list empty"

arrayHelper ::
    String ->
    ([Object.Object] -> Result Object.Object) ->
    [Object.Object] ->
    Result Object.Object
arrayHelper name fn arr
    | [Object.ArrObj arg] <- arr = fn arg
    | [other] <- arr = Object.failWith $ printf "argument to \"%s\" must be ARRAY, got %s" name $ Object.typeObject other
    | otherwise = Object.failWith $ printf "wrong number of arguments. got=%d, want=1" $ length arr

puts :: [Object.Object] -> Result Object.Object
puts objs = do
    mapM_ (liftIO . putStrLn . dprint) objs
    pure Object.Null
