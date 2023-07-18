module ParserSpec (spec) where

import Test.Hspec (Spec, SpecWith, describe, it, shouldBe)

import Ast
import Parser (parse)

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

spec :: Spec
spec = do
    testLetStatements
