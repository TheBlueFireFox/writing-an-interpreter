module Parser (parse) where

import Ast (Expression (..), Program (Program), Statement (..))
import Control.Arrow (first, second)
import Data.Bifunctor (Bifunctor (bimap))
import Data.Bool (bool)
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
    | Index
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
        LBracket -> Index
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
        inner (Token.Return : xs) = bimap ReturnStatement removeSemi <$> parseExpression Lowest xs
        inner xs = bimap ExpressionStatement removeSemi <$> parseExpression Lowest xs
     in
        inner

removeSemi :: [TokenType] -> [TokenType]
removeSemi cs = bool cs (tail cs) (head cs == Semicolon)

parseLetStatements :: [TokenType] -> Either [String] (Statement, [TokenType])
parseLetStatements [] = Left ["Missing Tokens"]
parseLetStatements (x : xs) =
    let
        ident (Token.Ident iden) = Right iden
        ident _ = Left ["Invalid Token Position"]

        expr [] = Left ["Empty expression"]
        expr (Token.Assign : cs) = second removeSemi <$> parseExpression Lowest cs
        expr (c : _) = Left ["Invalid stuff -- " ++ show c]
     in
        first . LetStatement <$> ident x <*> expr xs

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
    (Str val : cs) -> parseStr val cs
    (KTrue : cs) -> parseBool True cs
    (KFalse : cs) -> parseBool False cs
    (Bang : cs) -> parseNot cs
    (Minus : cs) -> parseNegative cs
    (LParen : cs) -> parseGrouped cs
    (LBracket : cs) -> parseArray cs
    (If : cs) -> parseIf cs
    (Function : cs) -> parseFnLit cs
    (c : _) -> Left ["Is not a prefix type " ++ show c]
    [] -> Left ["Emptry token list"]

parseExprList :: TokenType -> [TokenType] -> Either Errors ([Expression], [TokenType])
parseExprList stop toks =
    let
        parseNextExpr = parseExpression Lowest

        loop acc toks'
            | head toks' == Comma = uncurry loop . first (: acc) =<< parseNextExpr (tail toks')
            | head toks' == stop = Right (reverse acc, tail toks')
            | otherwise = Left ["unsupported token " ++ show toks' ++ " in call expression"]

        preCond toks'
            | head toks' == stop = Right ([], tail toks')
            -- loop
            | otherwise = uncurry loop . first (: []) =<< parseNextExpr toks'
     in
        preCond toks

parseInfixExpression :: Predecence -> Expression -> [TokenType] -> Either Errors (Expression, [TokenType])
parseInfixExpression _ left [] = Right (left, [])
parseInfixExpression pre left toks@(tok : xs)
    | tok == Semicolon = Right (left, toks)
    | pre >= mapPredecence tok = Right (left, toks)
    | otherwise =
        let
            runner_inner = uncurry (parseInfixExpression pre)
            runner p = runner_inner =<< parseInfix (p left) tok xs
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
                LParen -> runner_inner =<< parseCallExpression left xs
                LBracket -> runner_inner =<< parseIndexExpression left xs
                _ -> Right (left, toks)

parseArray :: [TokenType] -> Either Errors (Expression, [TokenType])
parseArray toks = first ArrExpr <$> parseExprList RBracket toks

parseIndexExpression :: Expression -> [TokenType] -> Either [String] (Expression, [TokenType])
parseIndexExpression left toks =
    let
        inner v = case v of
            Right (expr, RBracket : xs) -> Right (IndExpr left expr, xs)
            Right _ -> Left ["Missing RBracket"]
            err -> err
     in
        inner $ parseExpression Lowest toks

parseExpression :: Predecence -> [TokenType] -> Either Errors (Expression, [TokenType])
parseExpression pre xs = uncurry (parseInfixExpression pre) =<< parsePrefixExpression xs

parseCallExpression :: Expression -> [TokenType] -> Either Errors (Expression, [TokenType])
parseCallExpression left toks = first (CallExpr left) <$> parseExprList RParen toks

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

parseStr :: String -> [TokenType] -> Either Errors (Expression, [TokenType])
parseStr val xs = Right (StrExpr val, xs)

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
            helperIdentToks toks = case toks of
                [] -> Left ["Emptry token list"]
                (Ident ident : xs) -> Right (Ast.IdentExpr ident, xs)
                (x : _) -> Left ["Not a ident " ++ show x]

            helperParseParamsPreCond (acc, toks) = case toks of
                [] -> Left ["Emptry token list"]
                (LParen : toks') -> helperParseParamsLoopCond . first (: acc) =<< helperIdentToks toks'
                _ -> Left ["Weird token " ++ show toks]

            helperParseParamsLoopCond (acc, toks) = case toks of
                [] -> Left ["Emptry token list"]
                (RParen : _) -> Right (reverse acc, toks)
                (Comma : toks') -> helperParseParamsLoopCond . first (: acc) =<< helperIdentToks toks'
                _ -> Left ["Odd Token for params " ++ show (head toks)]

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
