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
removeSemi cs
    | (Semicolon : rest) <- cs = rest
    | otherwise = cs

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
        | current : rest <- toks', current == stop = Right ([], rest)
        -- loop
        -- add a fake comma to the list, so that the correct loop variant is used for
        -- processing
        | otherwise = loop [] (Comma : toks')

    loop acc toks'
        | [] <- toks' = Left ["Empty token list"]
        | Comma : rest <- toks' = do
            (expr, rest') <- parseExpression Lowest rest
            loop (expr : acc) rest'
        | current : rest <- toks', current == stop = Right (reverse acc, rest)
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
    | [] <- token = Left ["Empty token list"]
    | LParen : rest <- token = inner rest
    | otherwise = Left ["Missing left paren"]
  where
    inner token' = do
        (cond, toksOuter) <- parseExpression Lowest token'
        (cons, toksInner) <- helperCons toksOuter
        (alt, toks) <- helperAlt toksInner
        pure (IfExpr cond cons alt, toks)

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
        | [] <- token = Left ["Empty token list"]
        | LParen : _ <- token = helperInner token
        | otherwise = Left ["Missing left paren"]

    helperInner token = do
        (params, token') <- helperParseParams token
        (body, token'') <- parseBlockStatement token'
        pure (FnExpr params body, token'')

    helperIdentToks toks = case toks of
        [] -> Left ["Emptry token list"]
        (Ident ident : xs) -> Right (Ast.IdentExpr ident, xs)
        (x : _) -> Left ["Not a ident " ++ show x]

    helperParseParamsPreCond (acc, toks)
        | [] <- toks = Left ["Emptry token list"]
        | (LParen : rest@(RParen : _)) <- toks = Right ([], rest)
        | (LParen : toks') <- toks = helperParseParamsLoopCond . first (: acc) =<< helperIdentToks toks'
        | otherwise = Left ["Weird token " ++ show toks]

    helperParseParamsLoopCond (acc, toks)
        | [] <- toks = Left ["Emptry token list"]
        | RParen : _ <- toks = Right (reverse acc, toks)
        | Comma : rest <- toks = helperParseParamsLoopCond . first (: acc) =<< helperIdentToks rest
        | current : _ <- toks = Left ["Odd Token for params " ++ show current]

    helperParseParamsPostCond (expr, toks)
        | [] <- toks = Left ["Empty token list"]
        | RParen : LBrace : rest <- toks = Right (expr, rest)
        | RParen : _ <- toks = Left ["Missing RParen )" ++ show toks]
        | otherwise = Left ["Missing LBrace {" ++ show toks]

    helperParseParams toks = helperParseParamsPostCond =<< helperParseParamsPreCond ([], toks)

parseHashLit :: [TokenType] -> Either Errors (Expression, [TokenType])
parseHashLit t = inner (mempty, t)
  where
    inner (acc, tokens)
        | [] <- tokens = Left ["Empty token list"]
        | RBrace : rest <- tokens = Right (HashExpr (reverse acc), rest)
        | otherwise = inner =<< checkEnd acc =<< parseSecond =<< checkColon =<< parseFirst tokens

    checkColon (left, token)
        | [] <- token = Left ["Empty token list"]
        | Colon : rest <- token = Right (left, rest)
        | otherwise = Left ["Missing Colon"]

    parseFirst = parseExpression Lowest

    parseSecond (left, tokens) = first (left,) <$> parseExpression Lowest tokens

    checkEnd acc (pair, tokens)
        | [] <- tokens = Left ["Empty token list"]
        | Comma : rest <- tokens = Right (pair : acc, rest)
        | RBrace : _ <- tokens = Right (pair : acc, tokens)
        | current : _ <- tokens = Left ["unexpected token " ++ show current]
