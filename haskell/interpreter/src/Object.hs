module Object (module Object) where

import Ast (Display (dprint), Expression, Statement)
import Data.Bool (bool)
import Data.Int (Int64)
import Data.List (intercalate)
import Environment qualified

type Env = Environment.Env Object

type BuildInFunction = [Object.Object] -> Object.Object

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
    | RetObj Object
    | FnObj [Expression] Statement Env -- Params Body Env
    | BuiObj BuildInFunction -- Fn
    | ErrObj String
    deriving(Show, Eq)


typeObject :: Object -> String
typeObject obj = case obj of
    Null -> "NULL"
    IntObj _ -> "INTEGER"
    BoolObj _ -> "BOOLEAN"
    StrObj _ -> "STRING"
    ArrObj _ -> "ARRAY"
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
    dprint (FnObj params expr _) = "fn(" ++ intercalate ", " (map dprint params) ++ ")" ++ "{\n" ++ dprint expr ++ "\n}"
