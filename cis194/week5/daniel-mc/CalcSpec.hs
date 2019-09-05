module CalcSpec where

import           Calc
import           ExprT

import           Test.Hspec

main :: IO ()
main = hspec $ do
  describe "eval" $
    it "should work with nested expressions" $
      eval (Mul (Mul (Lit 2) (Lit 3)) (Add (Lit 1) (Lit 2))) `shouldBe` 18

  describe "evalStr" $ do
    it "should be Nothing for an invalid expression" $
      evalStr "2+" `shouldBe` Nothing

    it "should work for a valid expression" $
      evalStr "2+3*4" `shouldBe` Just 14
