module Repl (runner) where

import Ast (Display (dprint))
import Control.Monad.IO.Class (liftIO)
import Environment qualified as Env
import Evaluator (evalProgram)
import Object qualified
import Parser (parse)
import System.Console.Haskeline (InputT, defaultSettings, getInputLine, outputStrLn, runInputT)

prompt :: String
prompt = ">> "

runner :: IO ()
runner = do
    putStrLn "Hello! This is the Monkey programming language!"
    putStrLn "Feel free to type in commands"
    runInputT defaultSettings (replLoop =<< liftIO Env.newEnv)

replLoop :: Object.Env -> InputT IO ()
replLoop env = do
    minput <- getInputLine prompt
    case minput of
        Nothing -> outputStrLn "Bye"
        Just input -> replLoop =<< process env input

process :: Object.Env -> String -> InputT IO (Object.Env)
process env line
    | null line = pure env
    | otherwise = innerProcess env line

innerProcess :: Object.Env -> String -> InputT IO (Object.Env)
innerProcess oenv line = do
    evalResult <- liftIO $ eval oenv line
    case evalResult of
        Left errs -> do
            mapM_ outputStrLn errs
            pure oenv
        Right (Object.Null, env) -> pure env
        Right (others, env) -> do
            outputStrLn $ dprint $ others
            pure env

eval :: Object.Env -> String -> IO (Either [String] (Object.Object, Object.Env))
eval env input = case parse input of
    Left [] -> pure $ Left ["There was an error and the error list is empty"]
    Left errs -> pure $ Left errs
    Right prog -> Right <$> evalProgram env prog
