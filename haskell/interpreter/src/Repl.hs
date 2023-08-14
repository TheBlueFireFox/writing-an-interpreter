module Repl (runner) where

import GHC.IO.Handle (hFlush)
import System.IO (isEOF, stdout)

import Ast (Display (dprint))
import Evaluator (evalProgram)
import Object qualified
import Parser (parse)

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

eval :: String -> Object.Object
eval input = case parse input of
    Left err -> error $ head err
    Right prog -> evalProgram prog

runner :: IO ()
runner = do
    putStrLn "Hello! This is the Monkey programming language!"
    putStrLn "Feel free to type in commands"
    replLoop

replLoop :: IO ()
replLoop =
    do
        printPromt
        done <- isEOF
        if done
            then putStrLn "\nBye!"
            else process

process :: IO ()
process = do
    line <- getLine
    if line == "\n"
        then do
            printLine . dprint . eval $ line
            replLoop
        else replLoop
