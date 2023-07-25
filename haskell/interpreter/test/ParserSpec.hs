module ParserSpec (spec) where

import Test.Hspec

import Ast
import Parser

spec :: Spec
spec = do
    testLetStatements
    testReturnStatements
    testIdentifierExpression
    testIntegerLitteralsExpression
    testPrefixExpression
    testInfixExpression

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
                    Right . Ast.Program $
                        [ Ast.LetStatement "x" Ast.Invalid
                        , Ast.LetStatement "y" Ast.Invalid
                        , Ast.LetStatement "foobar" Ast.Invalid
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
                    (Right . Ast.Program)
                        [ Ast.ReturnStatement Ast.Invalid
                        , Ast.ReturnStatement Ast.Invalid
                        , Ast.ReturnStatement Ast.Invalid
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

        it "test operand precedence" $ do
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
                    ]

            [show . parse . fst $ i | i <- tests] `shouldBe` ["Right " ++ snd i | i <- tests]
