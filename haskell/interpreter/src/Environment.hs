module Environment (Env, newEnv, getEnv, setEnv) where

import Data.HashMap.Strict (HashMap, empty, insert, (!?))
import Object qualified

newtype Env = Env
    { store :: HashMap String Object.Object
    }
    deriving (Show, Eq)

newEnv :: Env
newEnv = Env{store = empty}

getEnv :: Env -> String -> Maybe Object.Object
getEnv (Env store) name = store !? name

setEnv :: Env -> String -> Object.Object -> Env
setEnv (Env store) name obj = Env{ store = insert name obj store }
