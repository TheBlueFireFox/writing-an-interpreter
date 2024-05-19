module Object (module Object) where

import Ast (Display (dprint), Expression, Statement)
import Data.Bifunctor (bimap)
import Data.Bool (bool)
import Data.Hashable (Hashable (..))
import Data.Int (Int64)
import Data.List (intercalate)
import Environment qualified
import Data.HashMap.Strict (HashMap, toList)

type Env = Environment.Env Object

type BuildInFunction = [Object.Object] -> IO Object.Object

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
    | HashObj (HashMap Object Object)
    | RetObj Object
    | FnObj [Expression] Statement Env -- Params Body Env
    | BuiObj BuildInFunction -- Fn
    | ErrObj String
    deriving (Show, Eq)

instance Hashable Object where
    hashWithSalt salt obj = process (obj, obj)
      where
        process = uncurry (*) . bimap (hashWithSalt salt . typeObject) hasher

        hasher obj' = case genHash salt obj' of
            Right v -> v
            Left err -> error err

genHash :: Int -> Object -> Either String Int
genHash salt obj = case obj of
    IntObj v -> Right $ hashWithSalt salt v
    StrObj v -> Right $ hashWithSalt salt v
    BoolObj v -> Right $ hashWithSalt salt v
    o -> Left $ "unusable as hash key: " ++ typeObject o

genSimpleHash :: Object -> Either String Int
genSimpleHash = Object.genHash 0


typeObject :: Object -> String
typeObject obj = case obj of
    Null -> "NULL"
    IntObj _ -> "INTEGER"
    BoolObj _ -> "BOOLEAN"
    StrObj _ -> "STRING"
    ArrObj _ -> "ARRAY"
    HashObj _ -> "HASH"
    RetObj _ -> "RETURN"
    ErrObj _ -> "ERROR"
    BuiObj _ -> "BUILTIN"
    FnObj{} -> "FN"

instance Display Object where
    dprint Null = "null"
    dprint (IntObj v) = show v
    dprint (BoolObj v) = bool "false" "true" v
    dprint (StrObj v) = v
    dprint (RetObj v) = "return " ++ dprint v
    dprint (ErrObj v) = "ERROR: " ++ v
    dprint (BuiObj _) = "builtin function"
    dprint (ArrObj arr) = "[" ++ intercalate ", " (map dprint arr) ++ "]"
    dprint (HashObj obj) = "{" ++ intercalate ", " (map (\(k,v) -> dprint k ++ ": " ++ dprint v) (toList obj)) ++ "}"
    dprint (FnObj params expr _) = "fn(" ++ intercalate ", " (map dprint params) ++ ")" ++ "{\n" ++ dprint expr ++ "\n}"
