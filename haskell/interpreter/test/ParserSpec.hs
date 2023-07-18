module ParserSpec (spec) where

import Test.Hspec (Spec, SpecWith, describe, it, shouldBe)

import Ast
import Parser (parse)

spec :: Spec
spec = do
    testLetStatements
    testReturnStatements

testLetStatements :: SpecWith ()
testLetStatements =
    describe "TestLetStatements" $ do
        it "TestLetStatements" $ do
            let
                input =
                    "\
                    \ let x = 5; \
                    \ let y = 10; \
                    \ let foobar = 838383; \
                    \"
                expected =
                    (Left . Program)
                        [ LetStatement "x" Invalid
                        , LetStatement "y" Invalid
                        , LetStatement "foobar" Invalid
                        ]
            parse input `shouldBe` expected

testReturnStatements :: SpecWith ()
testReturnStatements =
    describe "TestReturnStatements" $ do
        it "TestReturnStatements" $ do
            let
                input =
                    "\
                    \return 5;\
                    \return 10;\
                    \return 993322;\
                    \"
                expected =
                    (Left . Program)
                        [ ReturnStatement Invalid
                        , ReturnStatement Invalid
                        , ReturnStatement Invalid
                        ]
            parse input `shouldBe` expected
