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
parse = helper [] . Lexer.run
  where
    helper acc token = case token of
        [] -> Left ["undefined state missing tokens"]
        [Token.Eof] -> Right . Program . reverse $ acc
        toks -> uncurry helper . first (: acc) =<< parseStatement toks

parseStatement :: [TokenType] -> Either [String] (Statement, [TokenType])
parseStatement = inner
  where
    inner [] = Left ["Empty Statement"]
    inner (Token.Let : xs) = parseLetStatements xs
    inner (Token.Return : xs) = bimap ReturnStatement removeSemi <$> parseExpression Lowest xs
    inner xs = bimap ExpressionStatement removeSemi <$> parseExpression Lowest xs

removeSemi :: [TokenType] -> [TokenType]
removeSemi cs = bool cs (tail cs) (head cs == Semicolon)

parseLetStatements :: [TokenType] -> Either [String] (Statement, [TokenType])
parseLetStatements [] = Left ["Missing Tokens"]
parseLetStatements (x : xs) = first . LetStatement <$> ident x <*> expr xs
  where
    ident (Token.Ident iden) = Right iden
    ident _ = Left ["Invalid Token Position"]

    expr [] = Left ["Empty expression"]
    expr (Token.Assign : cs) = second removeSemi <$> parseExpression Lowest cs
    expr (c : _) = Left ["Invalid stuff -- " ++ show c]

parseBlockStatement :: [TokenType] -> Either Errors (Statement, [TokenType])
parseBlockStatement = inner []
  where
    inner acc (RBrace : cs) = Right (BlockStatement $ reverse acc, cs)
    inner _ (Eof : _) = Left ["Unexpected EOF"]
    inner _ [] = Left ["Emptry toks"]
    inner acc toks = uncurry inner . first (: acc) =<< parseStatement toks

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
    (LBrace : cs) -> parseHashLit cs
    (If : cs) -> parseIf cs
    (Function : cs) -> parseFnLit cs
    (c : _) -> Left ["Is not a prefix type " ++ show c]
    [] -> Left ["Emptry token list"]

parseExprList :: TokenType -> [TokenType] -> Either Errors ([Expression], [TokenType])
parseExprList stop = preCond
  where
    preCond toks'
        | head toks' == stop = Right ([], tail toks')
        -- loop
        -- add a fake comma to the list, so that the correct loop variant is used for
        -- processing
        | otherwise = loop [] (Comma : toks')

    loop acc toks'
        | head toks' == Comma = uncurry loop . first (: acc) =<< parseExpression Lowest (tail toks')
        | head toks' == stop = Right (reverse acc, tail toks')
        | otherwise = Left ["unsupported token " ++ show toks' ++ " in call expression"]

parseInfixExpression :: Predecence -> Expression -> [TokenType] -> Either Errors (Expression, [TokenType])
parseInfixExpression _ left [] = Right (left, [])
parseInfixExpression pre left toks@(tok : xs)
    | pre >= mapPredecence tok = Right (left, toks)
    | otherwise = case tok of
        Plus -> runner AddExpr
        Minus -> runner MinExpr
        Slash -> runner DivExpr
        Asterisk -> runner MulExpr
        Eq -> runner EqExpr
        NotEq -> runner NeqExpr
        Lt -> runner LeExpr
        Gt -> runner GtExpr
        LParen -> runner_raw =<< parseCallExpression left xs
        LBracket -> runner_raw =<< parseIndexExpression left xs
        Semicolon -> Right (left, toks)
        _ -> Right (left, toks)
  where
    runner_raw = uncurry (parseInfixExpression pre)
    runner p = runner_raw =<< parseInfix (p left) tok xs

parseArray :: [TokenType] -> Either Errors (Expression, [TokenType])
parseArray toks = first ArrExpr <$> parseExprList RBracket toks

parseIndexExpression :: Expression -> [TokenType] -> Either [String] (Expression, [TokenType])
parseIndexExpression left = inner . parseExpression Lowest
  where
    inner v = case v of
        Right (expr, RBracket : xs) -> Right (IndExpr left expr, xs)
        Right _ -> Left ["Missing RBracket"]
        err -> err

parseExpression :: Predecence -> [TokenType] -> Either Errors (Expression, [TokenType])
parseExpression pre xs = uncurry (parseInfixExpression pre) =<< parsePrefixExpression xs

parseCallExpression :: Expression -> [TokenType] -> Either Errors (Expression, [TokenType])
parseCallExpression left toks = first (CallExpr left) <$> parseExprList RParen toks

parseGrouped :: [TokenType] -> Either Errors (Expression, [TokenType])
parseGrouped xs = uncurry helper =<< parseExpression Lowest xs
  where
    helper expr (RParen : cs) = Right (expr, cs)
    helper _ _ = Left ["not a completed group"]

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
    | otherwise = do
        (cond, toksOuter) <- parseExpression Lowest (tail token)
        (cons, toksInner) <- helperCons toksOuter
        (alt, toks) <- helperAlt toksInner
        Right (IfExpr cond cons alt, toks)
  where
    helperCons (RParen : LBrace : tok) = parseBlockStatement tok
    helperCons _ = Left ["Missing {"]

    helperAlt (Else : LBrace : tok) = first Just <$> parseBlockStatement tok
    helperAlt (Else : _) = Left ["missing block statement else { x }"]
    helperAlt [] = Left ["no more tokens"]
    helperAlt toks = Right (Nothing, toks)

parseFnLit :: [TokenType] -> Either Errors (Expression, [TokenType])
parseFnLit = inner
  where
    inner token
        | null token = Left ["Empty token list"]
        | head token /= LParen = Left ["Missing left paren"]
        | otherwise = do
            (params, token') <- helperParseParams token
            (body, token'') <- parseBlockStatement token'
            Right (FnExpr params body, token'')

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

parseHashLit :: [TokenType] -> Either Errors (Expression, [TokenType])
parseHashLit t = inner (mempty, t)
  where
    inner (acc, token)
        | null token = Left ["Empty token list"]
        | head token == RBrace = Right (HashExpr (reverse acc), tail token)
        | otherwise = inner =<< checkEnd acc =<< parseSecond =<< checkColon =<< parseFirst token

    checkColon (left, token)
        | null token = Left ["Empty token list"]
        | head token /= Colon = Left ["Missing Colon"]
        | otherwise = Right (left, tail token)

    parseFirst = parseExpression Lowest

    parseSecond (left, tokens) = first (left,) <$> parseExpression Lowest tokens

    checkEnd acc (pair, tokens)
        | null tokens = Left ["Empty token list"]
        | head tokens == Comma = Right (pair : acc, tail tokens)
        | head tokens == RBrace = Right (pair : acc, tokens)
        | otherwise = Left ["unexpected token " ++ show (head tokens)]
