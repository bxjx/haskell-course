module Main where

import Test.Hspec

import Exercise1
import Exercise2
import Log

main :: IO ()
main = hspec $ do
  describe "validate" $ do
    describe "with an invalid card number" $ do
      it "returns false" $
        validate 4012888888881882 `shouldBe` False

    describe "with a valid card number" $ do
      it "returns true" $
        validate 4012888888881881 `shouldBe` True

  describe "parseMessage" $ do
    it "should detect errors" $
      parseMessage "E 2 562 help help" `shouldBe` LogMessage (Error 2) 562 "help help"
    it "should handle an invalid format" $
      parseMessage "E" `shouldBe` Unknown "invalid"
    it "should handle an invalid format" $
      parseMessage "Z 2 562 help help" `shouldBe` Unknown "invalid"

  describe "insert" $ do
    let earlierWarning = LogMessage Warning 2 "early"
        laterWarning = LogMessage Warning 4 "later"

    it "should return the same tree if insert with a Unknown" $
      insert (Unknown "test") Leaf `shouldBe` Leaf
    it "should return a node if insert with a non Unknown log message" $
      insert earlierWarning Leaf `shouldBe` Node Leaf earlierWarning Leaf
    it "should return sort the tree" $
      insert laterWarning (Node Leaf earlierWarning Leaf) `shouldBe` Node Leaf earlierWarning (Node Leaf laterWarning Leaf)
    it "should return sort the tree" $
      insert earlierWarning (Node Leaf laterWarning Leaf) `shouldBe` Node (Node Leaf earlierWarning Leaf) laterWarning Leaf
