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
    testBuildIns
    testArray
    testDrivingArrays

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

testArray :: SpecWith ()
testArray = do
    describe "TestArray" $ do
        it "overview" $ do
            let
                input = "[1, 2 * 2, 3 + 3]"
                expected =
                    Object.ArrObj
                        [ Object.IntObj v | v <- [1, 4, 6]
                        ]
            testEval input `shouldBe` expected
        it "index expressions" $ do
            let
                lst =
                    [ ("[1, 2, 3][0]", Object.IntObj 1)
                    , ("[1, 2, 3][1]", Object.IntObj 2)
                    , ("[1, 2, 3][2]", Object.IntObj 3)
                    , ("let i = 0; [1][i];", Object.IntObj 1)
                    , ("[1, 2, 3][1 + 1];", Object.IntObj 3)
                    , ("let myArray = [1, 2, 3]; myArray[2];", Object.IntObj 3)
                    , ("let myArray = [1, 2, 3]; let i = myArray[0]; myArray[i]", Object.IntObj 2)
                    , ("[1, 2, 3][3]", Object.Null)
                    , ("[1, 2, 3][-1]", Object.Null)
                    ]
                process = first testEval
                (got, expected) = unzip . map process $ lst
            got `shouldBe` expected
        it "index expressions with math" $ do
            let
                -- this test createed problems so here I am testing only it
                got = testEval "let myArray = [1, 2, 3]; myArray[0] + myArray[1] + myArray[2];"
                expected = Object.IntObj 6
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

testBuildIns :: SpecWith ()
testBuildIns =
    describe "TestBuildIns" $ do
        it "len" $ do
            let
                lst =
                    [ ("len(\"\")", Object.IntObj 0)
                    , ("len(\"four\")", Object.IntObj 4)
                    , ("len(\"hello world\")", Object.IntObj 11)
                    , ("len(1)", Object.ErrObj "argument to \"len\" not supported, got INTEGER")
                    , ("len(\"one\", \"two\")", Object.ErrObj "wrong number of arguments. got=2, want=1")
                    ]
                process = first testEval
                (got, expected) = unzip . map process $ lst
            got `shouldBe` expected

testDrivingArrays :: SpecWith ()
testDrivingArrays =
    describe "TestDrivingArrays" $ do
        it "map -- simple" $ do
            let
                inputMap =
                    "\
                    \let iter = fn(i) {                                 \
                    \   if(i > 0) {                                     \
                    \       iter(i - 1);                                \
                    \   } else {                                        \
                    \        42;                                        \
                    \   }                                               \
                    \ };                                                \
                    \"
                inputExpr = "iter(2);"
                input = inputMap ++ inputExpr

                expected = Object.IntObj 42
                got = testEval input
            got `shouldBe` expected
        it "map -- function in function" $ do
            let
                inputMap =
                    "\
                    \let map = fn(arr, f) {                                         \
                    \   let iter = fn(arr, accumulated) {                           \
                    \       if (len(arr) == 0) {                                    \
                    \          accumulated                                          \
                    \       } else {                                                \
                    \          iter(rest(arr), push(accumulated, f(first(arr))));   \
                    \       }                                                       \
                    \   };                                                          \
                    \   iter(arr, []);                                              \
                    \ };                                                            \
                    \"
                inputArr = "let a = [1, 2, 3, 4];"
                inputDouble = "let double = fn(x) { x * 2 };"
                inputExpr = "map(a, double);"
                input = inputMap ++ inputArr ++ inputDouble ++ inputExpr

                expected = Object.ArrObj $ [Object.IntObj v | v <- [2, 4, 6, 8]]
                got = testEval input
            got `shouldBe` expected
        it "reduce" $ do
            let
                reduceFn =
                    " \
                    \let reduce = fn(arr, initial, f) {                             \
                    \    let iter = fn(arr, result) {                               \
                    \        if (len(arr) == 0) {                                   \
                    \            result                                             \
                    \        } else {                                               \
                    \            iter(rest(arr), f(result, first(arr)));            \
                    \        }                                                      \
                    \    };                                                         \
                    \    iter(arr, initial);                                        \
                    \};                                                             \
                    \"
                sumFn =
                    "\
                    \ let sum = fn(arr) {                                            \
                    \     reduce(arr, 0, fn(initial, el) {                           \
                    \         initial + el                                           \
                    \     });                                                        \
                    \ };                                                             \
                    \"
                arr = "sum([1, 2, 3, 4, 5]);"
                input = reduceFn ++ sumFn ++ arr
                expected = Object.IntObj 15
                got = testEval input
            got `shouldBe` expected
