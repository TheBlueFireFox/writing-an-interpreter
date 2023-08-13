module Repl (runner) where

import Control.Monad (forever)
import GHC.IO.Handle (hFlush)
import System.IO (stdout)

import Evaluator (evalProgram)
import Object qualified
import Parser (parse)
import Ast (Display(dprint))

promt :: String
promt = ">> "

printPromt :: IO ()
printPromt = do
    putStr promt
    hFlush stdout

printLine :: String -> IO ()
printLine s = do
    putStrLn s
    hFlush stdout

eval :: String -> [Object.Object]
eval input = case parse input of
    Left err -> error $ head err
    Right prog -> evalProgram prog

runner :: IO ()
runner = forever $ do
    printPromt
    line <- getLine
    printLine $ (show . map dprint . eval) line
