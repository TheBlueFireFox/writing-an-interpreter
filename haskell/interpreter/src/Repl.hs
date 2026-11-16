module Repl (runner) where

import Ast (Display (dprint))
import Environment qualified as Env
import Evaluator (evalProgram)
import GHC.IO.Handle (hFlush)
import Object qualified
import Parser (parse)
import System.IO (isEOF, stdout)

prompt :: String
prompt = ">> "

printPrompt :: IO ()
printPrompt = do
    putStr prompt
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

eval :: Object.Env -> String -> IO (Either [String] (Object.Object, Object.Env))
eval env input = case parse input of
    Left [] -> pure $ Left ["There was an error and the error list is empty"]
    Left errs -> pure $ Left errs
    Right prog -> Right <$> evalProgram env prog

replLoop :: Object.Env -> IO ()
replLoop env = do
    printPrompt
    done <- isEOF
    case done of
        True -> putStrLn "\nBye!"
        False -> replLoop =<< process env =<< getLine

process :: Object.Env -> String -> IO (Object.Env)
process env line
    | null line = pure env
    | otherwise = innerProcess env line

innerProcess :: Object.Env -> String -> IO (Object.Env)
innerProcess oenv line = do
    evalResult <- eval oenv line
    case evalResult of
        Left errs -> do
            mapM_ printLine errs
            pure oenv
        Right (Object.Null, env) -> pure env
        Right (others, env) -> do
            printLine . dprint $ others
            pure env
