module Week4Spec where

  import           Week4

  import           Test.Hspec

  main :: IO ()
  main = hspec $ do
    describe "fun1" $ do
      it "should return 1 for empty list" $ fun1 [] `shouldBe` 1

      it "should return 1 for list of odd numbers" $ fun1 [1,3,5,7,9] `shouldBe` 1

      it "should return 0 for list with 2" $ fun1 [1,4,7,6,2,4,6,8,9] `shouldBe` 0

      it "should return product of even numbers minus 2" $ fun1 [1,4,7,6,4,6,8,9] `shouldBe` 384

    describe "fun2" $ do
      it "should return 0 for 1" $ fun2 1 `shouldBe` 0
      it "should return 2 for 2" $ fun2 2 `shouldBe` 2
      it "should return 40 for 3" $ fun2 3 `shouldBe` 40
      it "should return 46 for 6" $ fun2 6 `shouldBe` 46
      it "should return 234 for 7" $ fun2 7 `shouldBe` 234
