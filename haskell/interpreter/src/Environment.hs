module Environment (Env, newEnv, getEnv, setEnv, newEnclosedEnv) where

import Data.HashMap.Strict (HashMap, empty, insert, (!?))
import GHC.Base (Alternative ((<|>)))

data Env a = Env
    { store :: HashMap String a
    , parent :: Maybe (Env a)
    }
    deriving (Show, Eq)

newEnv :: Env a
newEnv = Env{store = empty, parent = Nothing}

newEnclosedEnv :: Env a -> Env a
newEnclosedEnv parent = Env{store = empty, parent = Just parent}

getEnv :: Env a -> String -> Maybe a
getEnv (Env store parent) name = (store !? name) <|> ((`getEnv` name) =<< parent)

setEnv :: Env a -> String -> a -> Env a
setEnv (Env store parent) name obj = Env{store = insert name obj store, parent}
