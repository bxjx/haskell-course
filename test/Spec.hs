module Main where

import Test.Hspec

import Exercise1

main :: IO ()
main = hspec $ do
  describe "validate" $ do
    describe "with an invalid card number" $ do
      it "returns false" $
        validate 4012888888881882 `shouldBe` False

    describe "with a valid card number" $ do
      it "returns true" $
        validate 4012888888881881 `shouldBe` True
