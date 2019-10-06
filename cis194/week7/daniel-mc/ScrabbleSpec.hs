module ScrabbleSpec where

  import           Scrabble

  import           Test.Hspec

  main :: IO ()
  main = hspec $ do
    describe "score" $ do
      it "scores lowercase correctly" $ score 'a' `shouldBe` 1
      it "scores uppercase correctly" $ score 'A' `shouldBe` 1
      it "scores non-letter" $ score '_' `shouldBe` 0

    describe "scoreString" $
      it "scores correctly" $ scoreString "yay Haskell!" `shouldBe` 23
