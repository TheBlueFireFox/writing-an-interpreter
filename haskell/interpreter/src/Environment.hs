module Environment (Env, newEnv, getEnv, setEnv, newEnclosedEnv) where

import Data.HashMap.Strict (HashMap, empty, insert, (!?))
import Data.IORef (IORef, newIORef, readIORef, modifyIORef)
import Data.Functor (($>))
import GHC.IO (unsafePerformIO)

data IEnv a = IEnv
    { store :: HashMap String a
    , parent :: Maybe (Env a)
    }
    deriving (Show, Eq)

type Env a = IORef (IEnv a)

newEnv :: IO (Env a)
newEnv = newIORef $ IEnv{store = empty, parent = Nothing}

newEnclosedEnv :: Env a -> IO (Env a)
newEnclosedEnv parent = newIORef $ IEnv{store = empty, parent = Just parent}

getEnv :: Env a -> String -> IO (Maybe a)
getEnv ref name = do
    (IEnv store parent) <- readIORef ref
    case store !? name of
        Just v -> pure $ pure v
        Nothing -> case parent of
            Nothing -> pure Nothing
            Just p -> getEnv p name

setEnv :: Env a -> String -> a -> IO (Env a)
setEnv ref name obj = modifyIORef ref f $> ref
 where
    f (IEnv store parent) = IEnv{store = insert name obj store, parent}

-- Technically speaking not safe, but because it is only used inside of tests
-- it might be good enough
instance (Show a) => Show (Env a) where
    show a = show (unsafePerformIO (readIORef a))
