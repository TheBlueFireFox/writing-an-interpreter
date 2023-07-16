module LexerSpec (spec) where

import Lexer (new, tokenize)
import Test.Hspec
import Token

spec :: Spec
spec = do
    describe "TestNextToken" $ do
        let

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
                lexer = new input
            (fst . tokenize) lexer `shouldBe` map Token expected
        it "complex tokenizing" $ do
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

                lexer = new input
            (fst . tokenize) lexer `shouldBe` map Token expected
