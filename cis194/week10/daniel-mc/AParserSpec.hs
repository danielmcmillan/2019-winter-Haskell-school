module ScrabbleSpec where

import           AParser

import           Test.Hspec

main :: IO ()
main = hspec $ do
  describe "instance Functor Parser" $ do
    it "should map posInt parser" $ runParser ((+2) <$> posInt) "10x" `shouldBe` Just (12, "x")
