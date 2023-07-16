module LexerSpec (spec) where

import Lexer (new, tokenize)
import Test.Hspec (Spec, SpecWith, describe, it, shouldBe)
import Token (Token (..), TokenType (..))

run :: String -> [Token]
run = fst . tokenize . new

testNextTokenSimple :: SpecWith ()
testNextTokenSimple =
    it "simple tokenizing" $ do
        let
            input = "=+(){},;"
            expected =
                [ Assign
                , Plus
                , LParen
                , RParen
                , LBrace
                , RBrace
                , Comma
                , Semicolon
                , Eof
                ]
        run input `shouldBe` map Token expected

testMissingTokens :: SpecWith ()
testMissingTokens =
    it "missing tokens" $ do
        let
            input =
                "\
                \ let five = 5; \
                \ let ten = 10;\
                \ let add = fn(x, y) {\
                \ x + y;\
                \ };\
                \ let result = add(five, ten);"

            expected =
                [ Let
                , Ident "five"
                , Assign
                , Int 5
                , Semicolon
                , Let
                , Ident "ten"
                , Assign
                , Int 10
                , Semicolon
                , Let
                , Ident "add"
                , Assign
                , Function
                , LParen
                , Ident "x"
                , Comma
                , Ident "y"
                , RParen
                , LBrace
                , Ident "x"
                , Plus
                , Ident "y"
                , Semicolon
                , RBrace
                , Semicolon
                , Let
                , Ident "result"
                , Assign
                , Ident "add"
                , LParen
                , Ident "five"
                , Comma
                , Ident "ten"
                , RParen
                , Semicolon
                , Eof
                ]
        run input `shouldBe` map Token expected

testNextTokenMoreSymbols :: SpecWith ()
testNextTokenMoreSymbols =
    it "more symbol tokens" $ do
        let
            input =
                "\
                \ let five = 5; \
                \ let ten = 10;\
                \ let add = fn(x, y) {\
                \ x + y;\
                \ };\
                \ let result = add(five, ten);\
                \!-/*5;\
                \5 < 10 > 5;\
                \"

            expected =
                [ Let
                , Ident "five"
                , Assign
                , Int 5
                , Semicolon
                , Let
                , Ident "ten"
                , Assign
                , Int 10
                , Semicolon
                , Let
                , Ident "add"
                , Assign
                , Function
                , LParen
                , Ident "x"
                , Comma
                , Ident "y"
                , RParen
                , LBrace
                , Ident "x"
                , Plus
                , Ident "y"
                , Semicolon
                , RBrace
                , Semicolon
                , Let
                , Ident "result"
                , Assign
                , Ident "add"
                , LParen
                , Ident "five"
                , Comma
                , Ident "ten"
                , RParen
                , Semicolon
                , Bang
                , Minus
                , Slash
                , Asterisk
                , Int 5
                , Semicolon
                , Int 5
                , Lt
                , Int 10
                , Gt
                , Int 5
                , Semicolon
                , Eof
                ]
        run input `shouldBe` map Token expected

testNextTokenAllKeywords :: SpecWith ()
testNextTokenAllKeywords =
    it "all keywords tokens" $ do
        let
            input =
                "\
                \ let five = 5; \
                \ let ten = 10;\
                \ let add = fn(x, y) {\
                \ x + y;\
                \ };\
                \ let result = add(five, ten);\
                \!-/*5;\
                \5 < 10 > 5;\
                \ if (5 < 10) { \
                \ return true;\
                \ } else {\
                \ return false;\
                \ }\
                \"

            expected =
                [ Let
                , Ident "five"
                , Assign
                , Int 5
                , Semicolon
                , Let
                , Ident "ten"
                , Assign
                , Int 10
                , Semicolon
                , Let
                , Ident "add"
                , Assign
                , Function
                , LParen
                , Ident "x"
                , Comma
                , Ident "y"
                , RParen
                , LBrace
                , Ident "x"
                , Plus
                , Ident "y"
                , Semicolon
                , RBrace
                , Semicolon
                , Let
                , Ident "result"
                , Assign
                , Ident "add"
                , LParen
                , Ident "five"
                , Comma
                , Ident "ten"
                , RParen
                , Semicolon
                , Bang
                , Minus
                , Slash
                , Asterisk
                , Int 5
                , Semicolon
                , Int 5
                , Lt
                , Int 10
                , Gt
                , Int 5
                , Semicolon
                , If
                , LParen
                , Int 5
                , Lt
                , Int 10
                , RParen
                , LBrace
                , Return
                , KTrue
                , Semicolon
                , RBrace
                , Else
                , LBrace
                , Return
                , KFalse
                , Semicolon
                , RBrace
                , Eof
                ]
        run input `shouldBe` map Token expected

testNextTokenComplete :: SpecWith ()
testNextTokenComplete =
    it "complete tokens" $ do
        let
            input =
                "\
                \ let five = 5; \
                \ let ten = 10;\
                \ let add = fn(x, y) {\
                \ x + y;\
                \ };\
                \ let result = add(five, ten);\
                \!-/*5;\
                \5 < 10 > 5;\
                \ if (5 < 10) { \
                \ return true;\
                \ } else {\
                \ return false;\
                \ }\
                \ 10 == 10;\
                \ 10 != 9 ;\
                \"

            expected =
                [ Let
                , Ident "five"
                , Assign
                , Int 5
                , Semicolon
                , Let
                , Ident "ten"
                , Assign
                , Int 10
                , Semicolon
                , Let
                , Ident "add"
                , Assign
                , Function
                , LParen
                , Ident "x"
                , Comma
                , Ident "y"
                , RParen
                , LBrace
                , Ident "x"
                , Plus
                , Ident "y"
                , Semicolon
                , RBrace
                , Semicolon
                , Let
                , Ident "result"
                , Assign
                , Ident "add"
                , LParen
                , Ident "five"
                , Comma
                , Ident "ten"
                , RParen
                , Semicolon
                , Bang
                , Minus
                , Slash
                , Asterisk
                , Int 5
                , Semicolon
                , Int 5
                , Lt
                , Int 10
                , Gt
                , Int 5
                , Semicolon
                , If
                , LParen
                , Int 5
                , Lt
                , Int 10
                , RParen
                , LBrace
                , Return
                , KTrue
                , Semicolon
                , RBrace
                , Else
                , LBrace
                , Return
                , KFalse
                , Semicolon
                , RBrace
                , Int 10
                , Eq
                , Int 10
                , Semicolon
                , Int 10
                , NotEq
                , Int 9
                , Semicolon
                , Eof
                ]
        run input `shouldBe` map Token expected

spec :: Spec
spec = do
    describe "TestNextToken" $ do
        testNextTokenSimple
        testMissingTokens
        testNextTokenMoreSymbols
        testNextTokenAllKeywords
        testNextTokenComplete
