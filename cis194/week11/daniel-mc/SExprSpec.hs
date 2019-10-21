module SExprSpec where

import           Data.Tree
import           AParser
import           SExpr

import           Test.Hspec

main :: IO ()
main = hspec $ do
  describe "zeroOrMore" $ do
    it "should parse multiple" $ runParser (zeroOrMore (char 'a')) "aaabc" `shouldBe` Just
      ("aaa", "bc")

    it "should succeed on parsing none" $ runParser (zeroOrMore (char 'b')) "aaabc" `shouldBe` Just
      ("", "aaabc")

  describe "oneOrMore" $ do
    it "should parse multiple" $ runParser (oneOrMore (char 'a')) "aaabc" `shouldBe` Just
      ("aaa", "bc")

    it "should fail on parsing none" $ runParser (oneOrMore (char 'b')) "aaabc" `shouldBe` Nothing

  describe "spaces" $ do
    it "should succeed with no space" $ runParser spaces "abc" `shouldBe` Just ("", "abc")

    it "should parse multiple space" $ runParser spaces "  abc" `shouldBe` Just ("  ", "abc")

  describe "ident" $ do
    it "should fail with non-alphanumeric" $ runParser ident "@123" `shouldBe` Nothing

    it "should fail with numeric" $ runParser ident "1a2bc abc" `shouldBe` Nothing

    it "should succeed with alpha" $ runParser ident "a1b2c abc" `shouldBe` Just ("a1b2c", " abc")

    it "should succeed with single alpha" $ runParser ident "a abc" `shouldBe` Just ("a", " abc")
