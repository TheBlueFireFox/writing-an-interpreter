module Object (module Object) where

import Ast (Display (dprint))
import Data.Int (Int64)

data Object
    = Null
    | IntObj Int64
    | BoolObj Bool
    deriving (Show, Eq)

instance Display Object where
    dprint Null = "null"
    dprint (IntObj v) = show v
    dprint (BoolObj v) = show v
