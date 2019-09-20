module JoinListSpec where

  import           JoinList
  import Data.Monoid(Sum(..))

  import           Test.Hspec

  main :: IO ()
  main = hspec $ do
    describe "+++" $ do
      it "appends empty" $
        (Empty +++ Empty :: JoinList (Sum Integer) Integer) `shouldBe` Append mempty Empty Empty

      it "appends single" $
        Single (Sum 10) 1 +++ Single (Sum 20) 2
        `shouldBe` Append (Sum 30) (Single (Sum 10) 1) (Single (Sum 20) 2)

      it "appends tree" $
        Empty +++ Append (Sum 30) (Single (Sum 10) 1) (Single (Sum 20) 2)
        `shouldBe` Append (Sum 30) Empty (Append (Sum 30) (Single (Sum 10) 1) (Single (Sum 20) 2))
