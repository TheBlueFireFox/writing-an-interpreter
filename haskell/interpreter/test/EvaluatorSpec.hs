module EvaluatorSpec (spec) where

import Test.Hspec

import Control.Arrow (first)
import Data.Bifunctor (Bifunctor (bimap))
import Evaluator (evalProgram)
import Object qualified
import Parser (parse)

spec :: Spec
spec = do
    testIntegerObj
    testBoolObj
    testBangObj

testEval :: String -> [Object.Object]
testEval input = case parse input of
    Left err -> error $ head err
    Right prog -> evalProgram prog

testIntegerObj :: SpecWith ()
testIntegerObj = do
    describe "TestIntergerObject" $ do
        it "simple integer" $ do
            let
                input =
                    [ "5"
                    , "10"
                    ]
                expected =
                    [ [Object.IntObj 5]
                    , [Object.IntObj 10]
                    ]
            map testEval input `shouldBe` expected
        it "negation" $ do
            let
                input =
                    ["-5", "-10"]
                expected =
                    [[Object.IntObj (-5)], [Object.IntObj (-10)]]
            map testEval input `shouldBe` expected
        it "evaluation" $ do
            let
                rawInput =
                    [ ("5 + 5 + 5 + 5 - 10", 10)
                    , ("2 * 2 * 2 * 2 * 2", 32)
                    , ("-50 + 100 + -50", 0)
                    , ("5 * 2 + 10", 20)
                    , ("5 + 2 * 10", 25)
                    , ("20 + 2 * -10", 0)
                    , ("50 / 2 * 2 + 10", 60)
                    , ("2 * (5 + 10)", 30)
                    , ("3 * 3 * 3 + 10", 37)
                    , ("3 * (3 * 3) + 10", 37)
                    , ("(5 + 10 * 2 + 15 / 3) * 2 + -10", 50)
                    ]
                (input, expected) = unzip rawInput

            map testEval input `shouldBe` map ((: []) . Object.IntObj) expected

testBoolObj :: SpecWith ()
testBoolObj = do
    describe "TestBoolenObj" $ do
        it "simple bool" $ do
            let
                input =
                    [ "true"
                    , "false"
                    ]
                expected =
                    [ [Object.BoolObj True]
                    , [Object.BoolObj False]
                    ]
            map testEval input `shouldBe` expected
        it "evaluation bool" $ do
            let
                rawInput =
                    [ ("true", True)
                    , ("false", False)
                    , ("true == true", True)
                    , ("false == false", True)
                    , ("true == false", False)
                    , ("true != false", True)
                    , ("false != true", True)
                    ]

                (input, expected) = unzip rawInput
            map testEval input `shouldBe` map ((: []) . Object.BoolObj) expected
        it "evaluation Integer" $ do
            let
                rawInput =
                    [ ("1 < 2", True)
                    , ("1 > 2", False)
                    , ("1 < 1", False)
                    , ("1 > 1", False)
                    , ("1 == 1", True)
                    , ("1 != 1", False)
                    , ("1 == 2", False)
                    , ("1 != 2", True)
                    ]

                (input, expected) = unzip rawInput
            map testEval input `shouldBe` map ((: []) . Object.BoolObj) expected

testBangObj :: SpecWith ()
testBangObj = do
    describe "TestBangObj" $ do
        it "bang all" $ do
            let
                lst =
                    [ ("!true", False)
                    , ("!false", True)
                    , ("!5", False)
                    , ("!!true", True)
                    , ("!!false", False)
                    , ("!!5", True)
                    ]
                process = bimap testEval (\c -> [Object.BoolObj c])
                (got, expected) = unzip . map process $ lst
            got `shouldBe` expected
