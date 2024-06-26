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
    replLoop =<< Env.newEnv

eval :: Object.Env -> String -> IO (Object.Object, Object.Env)
eval env input = case parse input of
    Left err -> error $ "There was an error : " ++ head err
    Right prog -> evalProgram env prog

replLoop :: Object.Env -> IO ()
replLoop env = do
    printPromt
    checkIfDone =<< isEOF
  where
    checkIfDone done
        | done = putStrLn "\nBye!"
        | otherwise = process env

process :: Object.Env -> IO ()
process env = handleLine =<< getLine
  where
    handleLine "\n" = replLoop env
    handleLine line = innerProcess env line

innerProcess :: Object.Env -> String -> IO ()
innerProcess oenv line = do
    (obj, env) <- eval oenv line
    case obj of
        Object.Null -> pure ()
        others -> printLine . dprint $ others
    replLoop env
