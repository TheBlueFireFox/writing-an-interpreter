module ObjectSpec (spec) where

import Data.Hashable (Hashable (hash))
import Object (Object (..))
import Test.Hspec (Spec, SpecWith, describe, it, shouldBe, shouldNotBe)

spec :: Spec
spec = do
    testStringHashKey
    testBooleanHashKey
    testIntegerHashKey

testStringHashKey :: SpecWith ()
testStringHashKey = do
    describe "TestStringHashKey" $ do
        let
            str = Object.StrObj
        it "test string equality hashing" $ do
            let
                hello1 = str "Hello World"
                hello2 = str "Hello World"
                diff1 = str "My name is johnny"
                diff2 = str "My name is johnny"

            hash hello1 `shouldBe` hash hello2
            hash diff1 `shouldBe` hash diff2
            hash diff1 `shouldNotBe` hash hello1
            hash diff2 `shouldNotBe` hash hello2

testBooleanHashKey :: SpecWith ()
testBooleanHashKey = do
    describe "TestBooleanHashKey" $ do
        let
            bool = Object.BoolObj
        it "test boolean equality hashing" $ do
            let
                true1 = bool True
                true2 = bool True
                false1 = bool False
                false2 = bool False

            hash true1 `shouldBe` hash true2
            hash false1 `shouldBe` hash false2
            hash false1 `shouldNotBe` hash true1
            hash false2 `shouldNotBe` hash true2

testIntegerHashKey :: SpecWith ()
testIntegerHashKey = do
    describe "testIntegerHashKey" $ do
        let
            int = Object.IntObj
        it "test int equality hashing" $ do
            let
                one1 = int 1
                one2 = int 1
                two1 = int 2
                two2 = int 2

            hash one1 `shouldBe` hash one2
            hash two1 `shouldBe` hash two2
            hash two1 `shouldNotBe` hash one1
            hash two2 `shouldNotBe` hash one2
