module ParserSpec (spec) where

import Test.Hspec

import Ast qualified
import Data.Bifunctor (Bifunctor (bimap, first))
import Parser (parse)

spec :: Spec
spec = do
    testLetStatements
    testReturnStatements
    testIdentifierExpression
    testIntegerLitteralsExpression
    testBooleanExpression
    testStringExpressions
    testPrefixExpression
    testInfixExpression
    testOperandPrecedence
    testIfExpression
    testArrayLitteralExpression
    testFunctionLitteralExpression
    testCallExpression
    testHashLiteralExpression

testLetStatements :: SpecWith ()
testLetStatements =
    describe "TestLetStatements" $ it "only constantes and identifiers" $ do
        let
            input =
                "\
                \ let x = 5; \
                \ let y = true; \
                \ let foobar = y; \
                \"
            expected =
                Right . Ast.Program $
                    [ Ast.LetStatement "x" $ Ast.IntegerExpr 5
                    , Ast.LetStatement "y" $ Ast.BooleanExpr True
                    , Ast.LetStatement "foobar" $ Ast.IdentExpr "y"
                    ]
        parse input `shouldBe` expected

testReturnStatements :: SpecWith ()
testReturnStatements =
    describe "TestReturnStatements" $ it "only constantes and identifiers" $ do
        let
            input =
                "\
                \return 5;\
                \return true;\
                \return y;\
                \"
            expected =
                (Right . Ast.Program)
                    [ Ast.ReturnStatement $ Ast.IntegerExpr 5
                    , Ast.ReturnStatement $ Ast.BooleanExpr True
                    , Ast.ReturnStatement $ Ast.IdentExpr "y"
                    ]
        parse input `shouldBe` expected

testIdentifierExpression :: SpecWith ()
testIdentifierExpression =
    describe "TestIdentifierExpression" $ do
        it "simple foobnar example" $ do
            let
                input = "foobar;"
                expected =
                    Right . Ast.Program $
                        [ Ast.ExpressionStatement $ Ast.IdentExpr "foobar"
                        ]
            parse input `shouldBe` expected
        it "simple foobnar example" $ do
            let
                input = "foobar;"
                expected =
                    Right . Ast.Program $
                        [ Ast.ExpressionStatement $ Ast.IdentExpr "foobar"
                        ]
            parse input `shouldBe` expected

testIntegerLitteralsExpression :: SpecWith ()
testIntegerLitteralsExpression =
    describe "TestIntegerLitteral" $ do
        it "simple number example" $ do
            let
                input = "5;"
                expected =
                    Right . Ast.Program $
                        [ Ast.ExpressionStatement $ Ast.IntegerExpr 5
                        ]
            parse input `shouldBe` expected
        it "more complex number example" $ do
            let
                input = "666;"
                expected =
                    Right . Ast.Program $
                        [ Ast.ExpressionStatement $ Ast.IntegerExpr 666
                        ]
            parse input `shouldBe` expected

testBooleanExpression :: SpecWith ()
testBooleanExpression =
    describe "TestBooleanExpression" $ do
        it "simple true" $ do
            let
                input = "true;"
                expected =
                    Right . Ast.Program $
                        [ Ast.ExpressionStatement $ Ast.BooleanExpr True
                        ]
            parse input `shouldBe` expected
        it "simple false" $ do
            let
                input = "false;"
                expected =
                    Right . Ast.Program $
                        [ Ast.ExpressionStatement $ Ast.BooleanExpr False
                        ]
            parse input `shouldBe` expected

testStringExpressions :: SpecWith ()
testStringExpressions =
    describe "TestStringExpressions" $ it "simple" $ do
        let
            input = "\"hello world\";"
            expected =
                Right . Ast.Program $
                    [ Ast.ExpressionStatement $ Ast.StrExpr "hello world"
                    ]
        parse input `shouldBe` expected

testPrefixExpression :: SpecWith ()
testPrefixExpression =
    describe "TextPrefixExpression" $ do
        it "prefix before integer with semicolon" $ do
            let
                expected =
                    [ Right . Ast.Program $ [Ast.ExpressionStatement x]
                    | x <-
                        [ Ast.NotExpr . Ast.IntegerExpr $ 5
                        , Ast.NegExpr . Ast.IntegerExpr $ 5
                        ]
                    ]
                gotten =
                    [ parse "!5;"
                    , parse "-5;"
                    ]
            expected `shouldBe` gotten
        it "prefix before integer without semicolon" $ do
            let
                expected =
                    [ Right . Ast.Program $ [Ast.ExpressionStatement x]
                    | x <-
                        [ Ast.NotExpr . Ast.IntegerExpr $ 5
                        , Ast.NegExpr . Ast.IntegerExpr $ 5
                        ]
                    ]
                gotten =
                    [ parse "!5"
                    , parse "-5"
                    ]
            expected `shouldBe` gotten
        it "prefix before ident" $ do
            let
                expected =
                    [ Right . Ast.Program $ [Ast.ExpressionStatement x]
                    | x <-
                        [ Ast.NegExpr . Ast.IdentExpr $ "foobar"
                        , Ast.NotExpr . Ast.IdentExpr $ "foobar"
                        ]
                    ]
                gotten =
                    [ parse "-foobar;"
                    , parse "!foobar;"
                    ]
            expected `shouldBe` gotten

testInfixExpression :: SpecWith ()
testInfixExpression =
    describe "TextInfixExpression" $ do
        it "all operands with int" $ do
            let
                five = Ast.IntegerExpr 5
                expected =
                    [ Right . Ast.Program $ [Ast.ExpressionStatement (x five five)]
                    | x <-
                        [ Ast.AddExpr
                        , Ast.MinExpr
                        , Ast.MulExpr
                        , Ast.DivExpr
                        , Ast.GtExpr
                        , Ast.LeExpr
                        , Ast.EqExpr
                        , Ast.NeqExpr
                        ]
                    ]
                gotten =
                    [ parse x
                    | x <-
                        [ "5 + 5;"
                        , "5 - 5;"
                        , "5 * 5;"
                        , "5 / 5;"
                        , "5 > 5;"
                        , "5 < 5;"
                        , "5 == 5;"
                        , "5 != 5;"
                        ]
                    ]
            expected `shouldBe` gotten
        it "boolean comparisions" $ do
            let
                expected =
                    [ Right . Ast.Program $ [Ast.ExpressionStatement x]
                    | x <-
                        [ Ast.EqExpr (Ast.BooleanExpr True) (Ast.BooleanExpr True)
                        , Ast.NeqExpr (Ast.BooleanExpr True) (Ast.BooleanExpr False)
                        , Ast.NeqExpr (Ast.BooleanExpr False) (Ast.BooleanExpr True)
                        , Ast.EqExpr (Ast.BooleanExpr False) (Ast.BooleanExpr False)
                        ]
                    ]
                gotten =
                    [ parse x
                    | x <-
                        [ "true == true"
                        , "true != false"
                        , "false != true"
                        , "false == false"
                        ]
                    ]
            expected `shouldBe` gotten

testOperandPrecedence :: SpecWith ()
testOperandPrecedence =
    describe "TestOperandPrecedence" $ it "test operand precedence" $ do
        let
            tests =
                [ ("-a * b", "((-a) * b)")
                , ("!-a", "(!(-a))")
                , ("a + b + c", "((a + b) + c)")
                , ("a + b - c", "((a + b) - c)")
                , ("a * b * c", "((a * b) * c)")
                , ("a * b / c", "((a * b) / c)")
                , ("a + b / c", "(a + (b / c))")
                , ("a + b * c + d / e - f", "(((a + (b * c)) + (d / e)) - f)")
                , ("3 + 4; -5 * 5", "(3 + 4)((-5) * 5)")
                , ("5 > 4 == 3 < 4", "((5 > 4) == (3 < 4))")
                , ("5 < 4 != 3 > 4", "((5 < 4) != (3 > 4))")
                , ("3 + 4 * 5 == 3 * 1 + 4 * 5", "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))")
                , ("true", "true")
                , ("false", "false")
                , ("3 > 5 == false", "((3 > 5) == false)")
                , ("3 < 5 == true", "((3 < 5) == true)")
                , ("1 + (2 + 3) + 4", "((1 + (2 + 3)) + 4)")
                , ("(5 + 5) * 2", "((5 + 5) * 2)")
                , ("2 / (5 + 5)", "(2 / (5 + 5))")
                , ("-(5 + 5)", "(-(5 + 5))")
                , ("!(true == true)", "(!(true == true))")
                , ("a + add(b * c) + d", "((a + add((b * c))) + d)")
                , ("add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8))", "add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)))")
                , ("add(a + b + c * d / f + g)", "add((((a + b) + ((c * d) / f)) + g))")
                , ("a * [1, 2, 3, 4][b * c] * d", "((a * ([1, 2, 3, 4][(b * c)])) * d)")
                , ("add(a * b[2], b[1], 2 * [1, 2][1])", "add((a * (b[2])), (b[1]), (2 * ([1, 2][1])))")
                ]

        [Ast.dprint <$> (parse . fst) i | i <- tests] `shouldBe` [Right $ snd i | i <- tests]

testIfExpression :: SpecWith ()
testIfExpression =
    let
        blk x = Ast.BlockStatement [Ast.ExpressionStatement $ Ast.IdentExpr x]
        ident = Ast.IdentExpr
        es = Ast.ExpressionStatement
     in
        describe "TestIfExpession" $ do
            it "test no else block" $ do
                let
                    input = "if (x < y) { x }"

                    expected =
                        Right . Ast.Program $
                            [ es $ Ast.IfExpr (Ast.LeExpr (ident "x") (ident "y")) (blk "x") Nothing
                            ]

                parse input `shouldBe` expected

            it "test with else block" $ do
                let
                    input = "if (x < y) { x } else { y }"
                    expected =
                        Right . Ast.Program $
                            [ es $ Ast.IfExpr (Ast.LeExpr (ident "x") (ident "y")) (blk "x") (Just (blk "y"))
                            ]

                parse input `shouldBe` expected

testArrayLitteralExpression :: SpecWith ()
testArrayLitteralExpression =
    describe "TestArrayLitterals" $ do
        it "arrayExpr" $ do
            let
                input = "[1, 2 * 2, 3 + 3]"
                es = Ast.ExpressionStatement
                int = Ast.IntegerExpr
                mul = Ast.MulExpr
                add = Ast.AddExpr
                expected =
                    Right . Ast.Program $
                        [es $ Ast.ArrExpr [int 1, mul (int 2) (int 2), add (int 3) (int 3)]]

            parse input `shouldBe` expected

        it "indexExpr" $ do
            let
                input = "myArray[1 + 1]"
                es = Ast.ExpressionStatement
                iden = Ast.IdentExpr
                int = Ast.IntegerExpr
                add = Ast.AddExpr
                expected =
                    Right . Ast.Program $
                        [es $ Ast.IndExpr (iden "myArray") (add (int 1) (int 1))]
            parse input `shouldBe` expected
        it "indexExpr2" $ do
            let
                input = "let myArray = [1, 2, 3]; myArray[0] + myArray[1] + myArray[2];"
                ls = Ast.LetStatement
                es = Ast.ExpressionStatement
                iden = Ast.IdentExpr
                int = Ast.IntegerExpr
                add = Ast.AddExpr
                indx = Ast.IndExpr
                arr = Ast.ArrExpr
                expected =
                    Right . Ast.Program $
                        [ ls "myArray" (arr [int 1, int 2, int 3])
                        , es $ add (add (indx (iden "myArray") (int 0)) (indx (iden "myArray") (int 1))) (indx (iden "myArray") (int 2))
                        ]
            parse input `shouldBe` expected

testFunctionLitteralExpression :: SpecWith ()
testFunctionLitteralExpression =
    let
        blk inner = Ast.BlockStatement [Ast.ExpressionStatement inner]
        ident = Ast.IdentExpr
        es = Ast.ExpressionStatement
     in
        describe "TestFuntionLitterals" $ it "simple function litterals" $ do
            let
                input = "fn(x, y) { x + y; }"
                expected =
                    Right . Ast.Program $
                        [ es $
                            Ast.FnExpr
                                [ ident "x"
                                , ident "y"
                                ]
                                (blk (Ast.AddExpr (ident "x") (ident "y")))
                        ]

            parse input `shouldBe` expected

testCallExpression :: SpecWith ()
testCallExpression =
    let
        ident = Ast.IdentExpr
        int = Ast.IntegerExpr
        es = Ast.ExpressionStatement
     in
        describe "TestCallExpression" $ it "call expression" $ do
            let
                input = "add(1, 2 * 3, 4 + 5);"
                expected =
                    Right . Ast.Program $
                        [ es $
                            Ast.CallExpr
                                (ident "add")
                                [ int 1
                                , Ast.MulExpr (int 2) (int 3)
                                , Ast.AddExpr (int 4) (int 5)
                                ]
                        ]

            parse input `shouldBe` expected

testHashLiteralExpression :: SpecWith ()
testHashLiteralExpression =
    describe "TestHashLiteralExpression" $ do
        let
            str = Ast.StrExpr
            int = Ast.IntegerExpr
            ex = Ast.ExpressionStatement
        it "empty hash literal" $ do
            let
                input = "{}"
                expected =
                    Right . Ast.Program $
                        [ ex $ Ast.HashExpr mempty
                        ]
            parse input `shouldBe` expected
        it "parsing hash literals" $ do
            let
                input = "{\"one\": 1, \"two\": 2, \"three\": 3}"
                expected =
                    Right . Ast.Program $
                        [ ex $
                            Ast.HashExpr $
                                map (bimap str int) [("one", 1), ("two", 2), ("three", 3)]
                        ]
            parse input `shouldBe` expected
        it "parsing hash literals" $ do
            let
                input = "{\"one\": 0 + 1, \"two\": 10 - 8, \"three\": 15 / 5}"
                expected =
                    Right . Ast.Program $
                        [ ex $
                            Ast.HashExpr $
                                map
                                    (first str)
                                    [ ("one", Ast.AddExpr (int 0) (int 1))
                                    , ("two", Ast.MinExpr (int 10) (int 8))
                                    , ("three", Ast.DivExpr (int 15) (int 5))
                                    ]
                        ]
            parse input `shouldBe` expected
