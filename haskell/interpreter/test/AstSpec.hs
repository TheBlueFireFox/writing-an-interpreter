module AstSpec (spec) where

import Ast (Expression (IdentExpr), Program (Program), Statement (LetStatement), Display(dprint))
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
    describe "TextAstString" $ do
        it "simple generation" $ do
            let
                expected = "let myVar = anotherVar;"
                input =
                    Program
                        [ LetStatement "myVar" $ IdentExpr "anotherVar"
                        ]
            dprint input `shouldBe` expected
