module Parser (parse) where

import Ast (Expression (Invalid), Program (Program), Statement (LetStatement, ReturnStatement))
import Lexer (run)
import Token (TokenType (..))

type Errors = [String]

data Parser = Parser
    { tokens :: [Token.TokenType]
    , errors :: Errors
    }
    deriving (Show, Eq)

parse :: String -> Either Program Errors
parse =
    let
        helper acc token = case token of
            [] -> undefined
            [Token.Eof] -> Left . Program $ reverse acc
            toks ->
                let
                    res = parseStatement toks
                 in
                    case res of
                        Left (statement, restToks) -> helper (statement : acc) restToks
                        Right errs -> Right errs
     in
        helper [] . Lexer.run

parseStatement :: [Token.TokenType] -> Either (Statement, [Token.TokenType]) Errors
parseStatement =
    let
        inner [] = Right ["Empty Statement"]
        inner (Token.Let : xs) = parseLetStatements xs
        inner (Token.Return : xs) = parseReturnStatements xs
        inner _ = undefined
     in
        inner

parseLetStatements :: [Token.TokenType] -> Either (Statement, [Token.TokenType]) Errors
parseLetStatements [] = Right ["Missing Tokens"]
parseLetStatements (x : xs) =
    let
        ident (Token.Ident iden) = Left iden
        ident _ = Left "Invalid Token Position"
        -- invliad expression
        -- TODO: skipping until a semicolon
        expr [] = Right "Empty expression"
        expr (Token.Assign : cs) = Left (Invalid, (tail . dropWhile (/= Semicolon)) cs)
        expr (c : _) = Right ("Invalid stuff -- " ++ show c)
     in
        case (ident x, expr xs) of
            (Left n, Left (e, s)) -> Left (LetStatement n e, s)
            (Left _, Right er) -> Right [er]
            (Right er, Left _) -> Right [er]
            (Right a, Right b) -> Right [a, b]

parseReturnStatements :: [Token.TokenType] -> Either (Statement, [Token.TokenType]) Errors
parseReturnStatements [] = Right ["Missing Token"]
parseReturnStatements xs =
    let
        expr [] = Right ["Empty expression"]
        expr cs = Left (Invalid, (tail . dropWhile (/= Semicolon)) cs)
     in
        case expr xs of
            Left (e, s) -> Left (ReturnStatement e, s)
            Right err -> Right err
