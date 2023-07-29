module Repl (runner) where

import Control.Monad (forever)
import GHC.IO.Handle (hFlush)
import System.IO (stdout)

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
runner = forever $ do
    printPromt
    line <- getLine
    printLine $ (show . parse) line
