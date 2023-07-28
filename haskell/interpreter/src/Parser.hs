module Parser (parse) where

import Ast (Expression (..), Program (Program), Statement (..))
import Control.Arrow (first)
import Data.Bifunctor (Bifunctor (bimap))
import Data.Int (Int64)
import Lexer (run)
import Token (TokenType (..))

type Errors = [String]

type ParseStatement = [Token.TokenType] -> Either Errors (Statement, [Token.TokenType])
type ParseExpression = [Token.TokenType] -> Either Errors (Expression, [Token.TokenType])

data Predecence
    = Lowest
    | Equals
    | LessGreater
    | Sum
    | Product
    | Prefix
    | Call
    deriving (Enum, Eq, Ord)

mapPredecence :: TokenType -> Predecence
mapPredecence xs =
    case xs of
        Eq -> Equals
        NotEq -> Equals
        Lt -> LessGreater
        Gt -> LessGreater
        Plus -> Sum
        Minus -> Sum
        Slash -> Product
        Asterisk -> Product
        _ -> Lowest

parse :: String -> Either Errors Program
parse str =
    let
        helper acc token = case token of
            [] -> Left ["undefined state missing tokens"]
            [Token.Eof] -> Right . Program . reverse $ acc
            toks -> uncurry helper . first (: acc) =<< parseStatement toks
     in
        helper [] . Lexer.run $ str

parseStatement :: ParseStatement
parseStatement =
    let
        inner [] = Left ["Empty Statement"]
        inner (Token.Let : xs) = parseLetStatements xs
        inner (Token.Return : xs) = parseReturnStatements xs
        inner xs = parseExpressionStatement xs
     in
        inner

parseLetStatements :: ParseStatement
parseLetStatements [] = Left ["Missing Tokens"]
parseLetStatements (x : xs) =
    let
        ident (Token.Ident iden) = Right iden
        ident _ = Left ["Invalid Token Position"]
        -- invliad expression
        -- TODO: We are skipping the expression until we encounter a semicolon
        expr [] = Left ["Empty expression"]
        expr (Token.Assign : cs) = Right (Invalid, (tail . dropWhile (/= Semicolon)) cs)
        expr (c : _) = Left ["Invalid stuff -- " ++ show c]
     in
        first . LetStatement <$> ident x <*> expr xs

parseReturnStatements :: ParseStatement
parseReturnStatements [] = Left ["Missing Token"]
parseReturnStatements xs =
    let
        expr [] = Left ["Empty expression"]
        -- invliad expression
        -- TODO: We are skipping the expression until we encounter a semicolon
        expr cs = Right (Invalid, (tail . dropWhile (/= Semicolon)) cs)
     in
        first ReturnStatement <$> expr xs

parseExpressionStatement :: ParseStatement
parseExpressionStatement [] = Left ["Empty expression"]
parseExpressionStatement xs =
    let
        removeSemi (Token.Semicolon : r) = r
        removeSemi r = r
     in
        bimap ExpressionStatement removeSemi <$> parseExpression Lowest xs

parsePrefixExpression :: [TokenType] -> Either Errors (Expression, [TokenType])
parsePrefixExpression x = case x of
    (Ident val : cs) -> parseIdent val cs
    (Int val : cs) -> parseInt64 val cs
    (KTrue : cs) -> parseBool True cs
    (KFalse : cs) -> parseBool False cs
    (Bang : cs) -> parseNot cs
    (Minus : cs) -> parseNegative cs
    (LParen : cs) -> parseGrouped cs
    (c : _) -> Left ["Is not a prefix type " ++ show c]
    [] -> Left ["Emptry token list"]

parseInfixExpression :: Predecence -> Expression -> [TokenType] -> Either Errors (Expression, [TokenType])
parseInfixExpression _ left [] = Right (left, [])
parseInfixExpression pre left toks@(tok : xs)
    | tok == Semicolon = Right (left, toks)
    | pre >= mapPredecence tok = Right (left, toks)
    | otherwise =
        let
            runner p = uncurry (parseInfixExpression pre) =<< parseInfix (p left) tok xs
         in
            case tok of
                Plus -> runner AddExpr
                Minus -> runner MinExpr
                Slash -> runner DivExpr
                Asterisk -> runner MulExpr
                Eq -> runner EqExpr
                NotEq -> runner NeqExpr
                Lt -> runner LeExpr
                Gt -> runner GtExpr
                _ -> Right (left, toks)

parseExpression :: Predecence -> ParseExpression
parseExpression pre xs = uncurry (parseInfixExpression pre) =<< parsePrefixExpression xs

parseGrouped :: [TokenType] -> Either [String] (Expression, [TokenType])
parseGrouped xs =
    let
        helper expr (RParen : cs) = Right (expr, cs)
        helper _ _ = Left ["not a completed group"]
     in
        uncurry helper =<< parseExpression Lowest xs

parseIdent :: String -> [TokenType] -> Either Errors (Expression, [TokenType])
parseIdent val xs = Right (IdentExpr val, xs)

parseInt64 :: Int64 -> [TokenType] -> Either Errors (Expression, [TokenType])
parseInt64 val xs = Right (IntegerExpr val, xs)

parseBool :: Bool -> [TokenType] -> Either Errors (Expression, [TokenType])
parseBool val xs = Right (BooleanExpr val, xs)

parseNot :: [TokenType] -> Either Errors (Expression, [TokenType])
parseNot xs = first NotExpr <$> parseExpression Prefix xs

parseNegative :: [TokenType] -> Either Errors (Expression, [TokenType])
parseNegative xs = first NegExpr <$> parseExpression Prefix xs

parseInfix :: (Expression -> c) -> TokenType -> [TokenType] -> Either [String] (c, [TokenType])
parseInfix _ _ [] = Left ["Missing tokens"]
parseInfix expr tok xs = first expr <$> parseExpression (mapPredecence tok) xs
