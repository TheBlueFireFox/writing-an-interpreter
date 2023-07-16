module Lexer (Lexer, new, tokenize) where

import Control.Applicative
import Data.Char (isDigit, isLower, isSpace, isUpper)
import Data.Maybe
import Token (Token (..), TokenType (..))

newtype Lexer = Lexer String
    deriving (Show)

new :: String -> Lexer
new = Lexer

tokenize :: Lexer -> ([Token], Lexer)
tokenize =
    let
        inner acc@(Token Eof : _) l = (reverse acc, l)
        inner acc l =
            let
                (a, b) = nextToken l
             in
                inner (b : acc) a
     in
        inner []


advanceLexer :: Lexer -> Int -> Lexer
advanceLexer (Lexer input) v = Lexer $ drop v input

nextToken :: Lexer -> (Lexer, Token)
nextToken l@(Lexer input) =
    let
        parseClean v = parserSymbols v <|> keywords v <|> readIdentifier v <|> readInteger v <|> Just (Illegal, 0)

        parser v len = (\(t, r) -> (t, r + len)) <$> parseClean v

        (token, lexerAdv) = fromJust $ uncurry parser $ skipWhitespace input
     in
        (advanceLexer l lexerAdv, Token token)

isLetter :: Char -> Bool
isLetter c = or [fun c | fun <- [isUpper, isLower, (== '_')]]

keywords :: String -> Maybe (TokenType, Int)
keywords str =
    let
        match v = case v of
            "fn" -> Just Function
            "let" -> Just Let
            _ -> Nothing
        s = takeWhile isLetter str
     in
        (,) <$> match s <*> Just (length s)

readIdentifier :: String -> Maybe (TokenType, Int)
readIdentifier s =
    let
        ident = takeWhile isLetter s
     in
        if (not . null) ident
            then Just (Ident ident, length ident)
            else Nothing

readInteger :: String -> Maybe (TokenType, Int)
readInteger s =
    let
        val = takeWhile isDigit s
     in
        Just (Int (read val), length val)

parserSymbols :: String -> Maybe (TokenType, Int)
parserSymbols x = case x of
    ('=' : _) -> Just (Assign, 1)
    (';' : _) -> Just (Semicolon, 1)
    ('(' : _) -> Just (LParen, 1)
    (')' : _) -> Just (RParen, 1)
    (',' : _) -> Just (Comma, 1)
    ('+' : _) -> Just (Plus, 1)
    ('{' : _) -> Just (LBrace, 1)
    ('}' : _) -> Just (RBrace, 1)
    [] -> Just (Eof, 0)
    _ -> Nothing

skipWhitespace :: String -> (String, Int)
skipWhitespace s =
    let
        (pre, str) = span isSpace s
     in
        (str, length pre)
