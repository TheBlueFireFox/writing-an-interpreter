module EvaluatorSpec (spec) where

import Test.Hspec

import Ast (Expression (..), Statement (..))
import Data.Bifunctor (Bifunctor (bimap), first)
import Environment (newEnv)
import Evaluator (evalProgram)
import Object qualified
import Parser (parse)

spec :: Spec
spec = do
    testIntegerObj
    testBoolObj
    testStrObj
    testBangObj
    testIfElse
    testReturn
    testErrorHandling
    testLet
    testFun

testEval :: String -> Object.Object
testEval input = case parse input of
    Left err -> error $ head err
    Right prog -> fst $ evalProgram newEnv prog

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
                    [ Object.IntObj 5
                    , Object.IntObj 10
                    ]
            map testEval input `shouldBe` expected
        it "negation" $ do
            let
                input =
                    ["-5", "-10"]
                expected =
                    [Object.IntObj (-5), Object.IntObj (-10)]
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

            map testEval input `shouldBe` map Object.IntObj expected

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
                    [ Object.BoolObj True
                    , Object.BoolObj False
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
            map testEval input `shouldBe` map Object.BoolObj expected
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
            map testEval input `shouldBe` map Object.BoolObj expected

testStrObj :: SpecWith ()
testStrObj =
    describe "TestStrObj" $ do
        it "simple Hello World!" $ do
            let
                input = "\"Hello World!\""
                expected = Object.StrObj "Hello World!"
            testEval input `shouldBe` expected
        it "concatination" $ do
            let
                input = "\"Hello\" + \" \" + \"World!\""
                expected = Object.StrObj "Hello World!"
            testEval input `shouldBe` expected

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
                process = bimap testEval Object.BoolObj
                (got, expected) = unzip . map process $ lst
            got `shouldBe` expected

testIfElse :: SpecWith ()
testIfElse = do
    describe "TestIfElse" $ do
        it "all" $ do
            let
                lst =
                    [ ("if (true) { 10 }", Object.IntObj 10)
                    , ("if (false) { 10 }", Object.Null)
                    , ("if (1) { 10 }", Object.IntObj 10)
                    , ("if (1 < 2) { 10 }", Object.IntObj 10)
                    , ("if (1 > 2) { 10 }", Object.Null)
                    , ("if (1 > 2) { 10 } else { 20 }", Object.IntObj 20)
                    , ("if (1 < 2) { 10 } else { 20 }", Object.IntObj 10)
                    ]
                process = first testEval
                (got, expected) = unzip . map process $ lst
            got `shouldBe` expected

testReturn :: SpecWith ()
testReturn = do
    describe "TestReturn" $ do
        it "overview" $ do
            let
                lst =
                    [ ("return 10;", Object.IntObj 10)
                    , ("return 10; 9;", Object.IntObj 10)
                    , ("return 2 * 5; 9;", Object.IntObj 10)
                    , ("9; return 2 * 5; 9;", Object.IntObj 10)
                    ]
                process = first testEval
                (got, expected) = unzip . map process $ lst
            got `shouldBe` expected
        it "blockStatements" $ do
            let
                input = "if (10 > 1) { if (10 > 1) { return 10; } return 1; }"
                expected = Object.IntObj 10
            testEval input `shouldBe` expected

testErrorHandling :: SpecWith ()
testErrorHandling = do
    describe "TestErrorHandling" $ do
        it "overview" $ do
            let
                lst =
                    [ ("5 + true;", "type mismatch: INTEGER + BOOLEAN")
                    , ("5 + true; 5;", "type mismatch: INTEGER + BOOLEAN")
                    , ("-true", "unknown operator: -BOOLEAN")
                    , ("\"Hello\" - \"World\"", "unknown operator: STRING - STRING")
                    , ("true + false;", "unknown operator: BOOLEAN + BOOLEAN")
                    , ("5; true + false; 5", "unknown operator: BOOLEAN + BOOLEAN")
                    , ("if (10 > 1) { true + false; }", "unknown operator: BOOLEAN + BOOLEAN")
                    , ("if (10 > 1) { if (10 > 1) { return true + false; } return 1;}", "unknown operator: BOOLEAN + BOOLEAN")
                    ]
                process = bimap testEval Object.ErrObj
                (got, expected) = unzip . map process $ lst
            got `shouldBe` expected
        it "identifiers" $ do
            let
                lst = [("foobar", "identifier not found: foobar")]
                process = bimap testEval Object.ErrObj
                (got, expected) = unzip . map process $ lst
            got `shouldBe` expected

testLet :: SpecWith ()
testLet = do
    describe "TestLetStatement" $ do
        it "overview" $ do
            let
                lst =
                    [ ("let a = 5; a;", 5)
                    , ("let a = 5 * 5; a;", 25)
                    , ("let a = 5; let b = a; b;", 5)
                    , ("let a = 5; let b = a; let c = a + b + 5; c;", 15)
                    ]
                process = bimap testEval Object.IntObj
                (got, expected) = unzip . map process $ lst
            got `shouldBe` expected

testFun :: SpecWith ()
testFun = do
    describe "TestFunctionStatement" $ do
        it "simple" $ do
            let
                input = "fn(x) { x + 2; };"
                blk =
                    [ ExpressionStatement
                        ( AddExpr (IdentExpr "x") (IntegerExpr 2)
                        )
                    ]
                expected = Object.FnObj [IdentExpr "x"] (BlockStatement blk) Environment.newEnv
            testEval input `shouldBe` expected
        it "overview" $ do
            let
                lst =
                    [ ("let identity = fn(x) { x; }; identity(5);", 5)
                    , ("let identity = fn(x) { return x; }; identity(5);", 5)
                    , ("let double = fn(x) { x * 2; }; double(5);", 10)
                    , ("let add = fn(x, y) { x + y; }; add(5, 5);", 10)
                    , ("let add = fn(x, y) { x + y; }; add(5 + 5, add(5, 5));", 20)
                    , ("fn(x) { x; }(5)", 5)
                    ]
                process = bimap testEval Object.IntObj
                (got, expected) = unzip . map process $ lst
            got `shouldBe` expected
        it "closures" $ do
            let
                input = "let newAdder = fn(x) { fn(y) { x + y }; }; let addTwo = newAdder(2); addTwo(2)"
                expected = Object.IntObj 4
            testEval input `shouldBe` expected
