module Lexer (Lexer, new, tokenize, run) where

import Control.Applicative (Alternative ((<|>)))
import Control.Arrow (Arrow (first, second))
import Data.Char (isDigit, isLower, isSpace, isUpper)
import Data.Maybe (fromJust)
import Data.Tuple (swap)
import Token (TokenType (..))

newtype Lexer = Lexer String
    deriving (Show, Eq)

run :: String -> [TokenType]
run = fst . tokenize . new

new :: String -> Lexer
new = Lexer

tokenize :: Lexer -> ([TokenType], Lexer)
tokenize =
    let
        inner (Illegal v : _) _ = error $ "Illegal character -- " ++ v
        inner acc@(Eof : _) l = (reverse acc, l)
        inner acc l = (uncurry inner . swap . second (: acc) . nextToken) l
     in
        inner []

advanceLexer :: Lexer -> Int -> Lexer
advanceLexer (Lexer input) v = Lexer $ drop v input

nextToken :: Lexer -> (Lexer, TokenType)
nextToken l@(Lexer input) =
    let
        endOfFile v = case v of
            [] -> Just (Eof, 0)
            _ -> Nothing

        parseClean v = endOfFile v <|> parserSymbols v <|> keywords v <|> readIdentifier v <|> readInteger v <|> Just (Illegal v, 0)

        parser v len = second (+ len) <$> parseClean v

        runHelper = swap . fromJust . uncurry parser . skipWhitespace
     in
        first (advanceLexer l) $ runHelper input

isLetter :: Char -> Bool
isLetter c = or [fun c | fun <- [isUpper, isLower, (== '_')]]

keywords :: String -> Maybe (TokenType, Int)
keywords str =
    let
        match v = case v of
            "fn" -> Just Function
            "let" -> Just Let
            "true" -> Just KTrue
            "false" -> Just KFalse
            "if" -> Just If
            "else" -> Just Else
            "return" -> Just Return
            _ -> Nothing
        s = takeWhile isLetter str
     in
        (,) <$> match s <*> Just (length s)

readBlock :: (a1 -> Bool) -> ([a1] -> a2) -> [a1] -> Maybe (a2, Int)
readBlock p f =
    let
        helper [] = Nothing
        helper b = Just (f b, length b)
     in
        helper . takeWhile p

readIdentifier :: String -> Maybe (TokenType, Int)
readIdentifier = readBlock isLetter Ident

readInteger :: String -> Maybe (TokenType, Int)
readInteger = readBlock isDigit (Int . read)

parserSymbols :: String -> Maybe (TokenType, Int)
parserSymbols x = case x of
    ('=' : '=' : _) -> Just (Eq, 2)
    ('!' : '=' : _) -> Just (NotEq, 2)
    ('=' : _) -> Just (Assign, 1)
    ('+' : _) -> Just (Plus, 1)
    ('-' : _) -> Just (Minus, 1)
    ('*' : _) -> Just (Asterisk, 1)
    ('!' : _) -> Just (Bang, 1)
    ('/' : _) -> Just (Slash, 1)
    ('>' : _) -> Just (Gt, 1)
    ('<' : _) -> Just (Lt, 1)
    (';' : _) -> Just (Semicolon, 1)
    ('(' : _) -> Just (LParen, 1)
    (')' : _) -> Just (RParen, 1)
    (',' : _) -> Just (Comma, 1)
    ('{' : _) -> Just (LBrace, 1)
    ('}' : _) -> Just (RBrace, 1)
    _ -> Nothing

skipWhitespace :: String -> (String, Int)
skipWhitespace = second length . swap . span isSpace
