module Object (module Object) where

import Ast (Display (dprint), Expression, Statement)
import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.Trans.Except qualified as Except
import Data.Bool (bool)
import Data.HashMap.Strict (HashMap, toList)
import Data.Hashable (Hashable)
import Data.Int (Int64)
import Data.List (intercalate)
import Environment qualified
import GHC.Generics (Generic)

type Env = Environment.Env Object

failWith :: (Monad m) => String -> ExceptT Abandon m a
failWith s = Except.throwE $ Error s

data Abandon = Error String

type Result a = ExceptT Abandon IO a

type BuildInFunction = [Object.Object] -> Result Object.Object

-- We don't care about this here
instance Show BuildInFunction where
    show _ = "BUILTIN"

-- We don't really care about this
instance Eq BuildInFunction where
    (==) _ _ = True

data Object
    = Null
    | IntObj Int64
    | BoolObj Bool
    | StrObj String
    | ArrObj [Object]
    | HashObj (HashMap HashableObject Object)
    | RetObj Object
    | FnObj [Expression] Statement Env -- Params Body Env
    | BuiObj BuildInFunction -- Fn
    deriving (Show, Eq)

data HashableObject
    = IntKey Int64
    | BoolKey Bool
    | StrKey String
    deriving (Show, Eq, Generic)

instance Hashable HashableObject

toHashKey :: Object -> Either String HashableObject
toHashKey obj = case obj of
    IntObj v -> pure $ IntKey v
    StrObj v -> pure $ StrKey v
    BoolObj v -> pure $ BoolKey v
    o -> Left $ "unusable as hash key: " ++ typeObject o

fromHashKey :: HashableObject -> Object
fromHashKey key = case key of
    IntKey v -> IntObj v
    BoolKey v -> BoolObj v
    StrKey v -> StrObj v

typeObject :: Object -> String
typeObject obj = case obj of
    Null -> "NULL"
    IntObj _ -> "INTEGER"
    BoolObj _ -> "BOOLEAN"
    StrObj _ -> "STRING"
    ArrObj _ -> "ARRAY"
    HashObj _ -> "HASH"
    RetObj _ -> "RETURN"
    BuiObj _ -> "BUILTIN"
    FnObj{} -> "FN"

instance Display Object where
    dprint Null = "null"
    dprint (IntObj v) = show v
    dprint (BoolObj v) = bool "false" "true" v
    dprint (StrObj v) = v
    dprint (RetObj v) = "return " ++ dprint v
    dprint (BuiObj _) = "builtin function"
    dprint (ArrObj arr) = "[" ++ intercalate ", " (map dprint arr) ++ "]"
    dprint (HashObj obj) = "{" ++ intercalate ", " (map (\(k, v) -> dprint (fromHashKey k) ++ ": " ++ dprint v) (toList obj)) ++ "}"
    dprint (FnObj params expr _) = "fn(" ++ intercalate ", " (map dprint params) ++ ")" ++ "{\n" ++ dprint expr ++ "\n}"
