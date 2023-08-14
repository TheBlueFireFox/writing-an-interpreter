module Object (module Object) where

import Ast (Display (dprint))
import Data.Int (Int64)

data Object
    = Null
    | IntObj Int64
    | BoolObj Bool
    | RetObj Object
    | ErrObj String
    deriving (Show, Eq)

typeObject :: Object -> String
typeObject obj = case obj of
    Null -> "NULL"
    IntObj _ -> "INTEGER"
    BoolObj _ -> "BOOLEAN"
    RetObj _ -> "RETURN"
    ErrObj _ -> "ERROR"

instance Display Object where
    dprint Null = "null"
    dprint (IntObj v) = show v
    dprint (BoolObj v) = show v
    dprint (RetObj v) = "return " ++ dprint v
    dprint (ErrObj v) = v
