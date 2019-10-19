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
