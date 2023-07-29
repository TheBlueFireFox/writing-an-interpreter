module Parser (parse) where

import Ast (Expression (..), Program (Program), Statement (..))
import Control.Arrow (first, second)
import Data.Bifunctor (Bifunctor (bimap))
import Data.Int (Int64)
import Lexer (run)
import Token (TokenType (..))

type Errors = [String]

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
        LParen -> Call
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

parseStatement :: [TokenType] -> Either [String] (Statement, [TokenType])
parseStatement =
    let
        inner [] = Left ["Empty Statement"]
        inner (Token.Let : xs) = parseLetStatements xs
        inner (Token.Return : xs) = parseReturnStatements xs
        inner xs = parseExpressionStatement xs
     in
        inner

parseLetStatements :: [TokenType] -> Either [String] (Statement, [TokenType])
parseLetStatements [] = Left ["Missing Tokens"]
parseLetStatements (x : xs) =
    let
        removeSemi (Semicolon : cs) = cs
        removeSemi cs = cs

        ident (Token.Ident iden) = Right iden
        ident _ = Left ["Invalid Token Position"]

        expr [] = Left ["Empty expression"]
        expr (Token.Assign : cs) = second removeSemi <$> parseExpression Lowest cs
        expr (c : _) = Left ["Invalid stuff -- " ++ show c]
     in
        first . LetStatement <$> ident x <*> expr xs

parseReturnStatements :: [TokenType] -> Either [String] (Statement, [TokenType])
parseReturnStatements [] = Left ["Missing Token"]
parseReturnStatements xs =
    let
        removeSemi (Semicolon : cs) = cs
        removeSemi cs = cs

        expr cs = second removeSemi <$> parseExpression Lowest cs
     in
        first ReturnStatement <$> expr xs

parseExpressionStatement :: [TokenType] -> Either [String] (Statement, [TokenType])
parseExpressionStatement [] = Left ["Empty expression"]
parseExpressionStatement xs =
    let
        removeSemi (Token.Semicolon : r) = r
        removeSemi r = r
     in
        bimap ExpressionStatement removeSemi <$> parseExpression Lowest xs

parseBlockStatement :: [TokenType] -> Either Errors (Statement, [TokenType])
parseBlockStatement xs =
    let
        inner acc (RBrace : cs) = Right (BlockStatement $ reverse acc, cs)
        inner _ (Eof : _) = Left ["Unexpected EOF"]
        inner _ [] = Left ["Emptry toks"]
        inner acc toks = uncurry inner . first (: acc) =<< parseStatement toks
     in
        inner [] xs

parsePrefixExpression :: [TokenType] -> Either Errors (Expression, [TokenType])
parsePrefixExpression x = case x of
    (Ident val : cs) -> parseIdent val cs
    (Int val : cs) -> parseInt64 val cs
    (KTrue : cs) -> parseBool True cs
    (KFalse : cs) -> parseBool False cs
    (Bang : cs) -> parseNot cs
    (Minus : cs) -> parseNegative cs
    (LParen : cs) -> parseGrouped cs
    (If : cs) -> parseIf cs
    (Function : cs) -> parseFnLit cs
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
                LParen -> parseCallExpression left xs
                _ -> Right (left, toks)

parseExpression :: Predecence -> [TokenType] -> Either Errors (Expression, [TokenType])
parseExpression pre xs = uncurry (parseInfixExpression pre) =<< parsePrefixExpression xs

parseCallExpression :: Expression -> [TokenType] -> Either Errors (Expression, [TokenType])
parseCallExpression left toks =
    let
        parseNextExpr = parseExpression Lowest

        helperInner acc toks'
            | head toks' == Comma = uncurry helperInner . first (: acc) =<< parseNextExpr (tail toks')
            | head toks' == RParen = Right (reverse acc, tail toks')
            | otherwise = Left ["unsupported token " ++ show toks' ++ " in call expression"]

        helperOuter toks'
            | head toks' == RParen = Right ([], tail toks')
            -- loop
            | otherwise = uncurry helperInner . first (: []) =<< parseNextExpr toks'
     in
        first (CallExpr left) <$> helperOuter toks

parseGrouped :: [TokenType] -> Either Errors (Expression, [TokenType])
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

parseIf :: [TokenType] -> Either Errors (Expression, [TokenType])
parseIf token
    | null token = Left ["Empty token list"]
    | head token /= LParen = Left ["Missing left paren"]
    | otherwise =
        let
            helperCons (RParen : LBrace : tok) = parseBlockStatement tok
            helperCons _ = Left ["Missing {"]

            helperAlt (Else : LBrace : tok) = first Just <$> parseBlockStatement tok
            helperAlt (Else : _) = Left ["missing block statement else { x }"]
            helperAlt [] = Left ["no more tokens"]
            helperAlt toks = Right (Nothing, toks)
         in
            do
                (cond, toksOuter) <- parseExpression Lowest (tail token)
                (cons, toksInner) <- helperCons toksOuter
                (alt, toks) <- helperAlt toksInner
                Right (IfExpr cond cons alt, toks)

parseFnLit :: [TokenType] -> Either Errors (Expression, [TokenType])
parseFnLit token
    | null token = Left ["Empty token list"]
    | head token /= LParen = Left ["Missing left paren"]
    | otherwise =
        let
            helperIdentToks (Ident ident : xs) = Right (Ast.IdentExpr ident, xs)
            helperIdentToks (x : _) = Left ["Not a ident " ++ show x]
            helperIdentToks [] = Left ["Emptry token list"]

            helperParseParamsPreCond (acc, toks)
                | null toks = Left ["Emptry token list"]
                | head toks == LParen = helperParseParamsLoopCond . first (: acc) =<< helperIdentToks (tail toks)
                | otherwise = Left ["Weird token " ++ show toks]

            helperParseParamsLoopCond (acc, toks)
                | null toks = Left ["Emptry token list"]
                | head toks == RParen = Right (reverse acc, toks)
                | head toks == Comma = helperParseParamsLoopCond . first (: acc) =<< helperIdentToks (tail toks)
                | otherwise = Left ["Odd Token for params " ++ show (head toks)]

            helperParseParamsPostCond (expr, toks)
                | null toks = Left ["Empty token list"]
                | head toks /= RParen = Left ["Missing RParen )" ++ show toks]
                | toks !! 1 /= LBrace = Left ["Missing LBrace {" ++ show toks]
                | otherwise = Right (expr, drop 2 toks)

            helperParseParams toks = helperParseParamsPostCond =<< helperParseParamsPreCond ([], toks)
         in
            do
                (params, token') <- helperParseParams token
                (body, token'') <- parseBlockStatement token'
                Right (FnExpr params body, token'')
