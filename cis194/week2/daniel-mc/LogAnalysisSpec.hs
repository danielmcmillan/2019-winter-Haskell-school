module LogAnalysisSpec where

import Log
import LogAnalysis

import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "parseMessage" $ do
    it "should parse an error message" $
      parseMessage "E 2 562 help help" `shouldBe` LogMessage (Error 2) 562 "help help"

    it "should parse an info message" $
      parseMessage "I 29 la la la" `shouldBe` LogMessage Info 29 "la la la"

    it "should be Unknown for an invalid message" $
      parseMessage "This is not in the right format"
      `shouldBe` Unknown "This is not in the right format"

    it "should be Unknown for an invalid error level" $
      parseMessage "E ab 29 la la la" `shouldBe` Unknown "E ab 29 la la la"

    it "should be Unknown for an invalid timestamp" $
      parseMessage "I ab la la" `shouldBe` Unknown "I ab la la"

  describe "parseFile" $
    it "should parse a file of log messages" $
      parseFile "I 1 ab\nE 2 5 error" `shouldBe` [
        LogMessage Info 1 "ab",
        LogMessage (Error 2) 5 "error"
      ]
