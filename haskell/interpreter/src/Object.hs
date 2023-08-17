module Object (module Object) where

import Ast (Display (dprint), Expression, Statement)
import Data.Bool (bool)
import Data.Int (Int64)
import Data.List (intercalate)
import Environment qualified

type Env = Environment.Env Object

data Object
    = Null
    | IntObj Int64
    | BoolObj Bool
    | StrObj String
    | RetObj Object
    | FnObj [Expression] Statement Env -- Params Body Env
    | ErrObj String
    deriving (Show, Eq)

typeObject :: Object -> String
typeObject obj = case obj of
    Null -> "NULL"
    IntObj _ -> "INTEGER"
    BoolObj _ -> "BOOLEAN"
    StrObj _ -> "STRING"
    RetObj _ -> "RETURN"
    ErrObj _ -> "ERROR"
    FnObj{} -> "FN"

instance Display Object where
    dprint Null = "null"
    dprint (IntObj v) = show v
    dprint (BoolObj v) = bool "false" "true" v
    dprint (StrObj v) = v
    dprint (RetObj v) = "return " ++ dprint v
    dprint (ErrObj v) = "ERROR: " ++ v
    dprint (FnObj params expr _) = "fn(" ++ intercalate ", " (map dprint params) ++ ")" ++ "{\n" ++ dprint expr ++ "\n}"
