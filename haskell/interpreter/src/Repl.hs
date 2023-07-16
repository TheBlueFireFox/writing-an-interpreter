module Repl (runner) where

import Control.Monad (forever)
import GHC.IO.Handle (hFlush)
import System.IO (stdout)

import Lexer (new, tokenize)

promt :: String
promt = ">> "

printPromt :: IO ()
printPromt = do
    putStr promt
    hFlush stdout

printLine :: String -> IO ()
printLine s = do
    putStr s
    hFlush stdout

runner :: IO ()
runner = forever $ do
    printPromt
    line <- getLine
    printLine $ concatMap (\v -> "{ " ++ show v ++ " } \n") $ (fst . tokenize . new) line
