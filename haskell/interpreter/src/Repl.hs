
module Repl (runner) where

import GHC.IO.Handle (hFlush)
import System.IO (isEOF, stdout)

import Ast (Display (dprint))
import Environment qualified as Env
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

runner :: IO ()
runner = do
    putStrLn "Hello! This is the Monkey programming language!"
    putStrLn "Feel free to type in commands"
    replLoop Env.newEnv

eval :: Env.Env -> String -> (Object.Object, Env.Env)
eval env input = case parse input of
    Left err -> error $ "There was an error : " ++ head err
    Right prog -> evalProgram env prog

replLoop :: Env.Env -> IO ()
replLoop env =
    do
        printPromt
        done <- isEOF
        if done
            then putStrLn "\nBye!"
            else process env

process :: Env.Env -> IO ()
process env = do
    line <- getLine
    if line == "\n"
        then replLoop env
        else innerProcess env line

innerProcess :: Env.Env -> String -> IO ()
innerProcess oenv line = do
    let
        (obj, env) = eval oenv line
    case obj of
        Object.Null -> pure ()
        others -> printLine . dprint $ others
    replLoop env
