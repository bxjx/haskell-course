module Main where

import Test.Hspec

import Exercise1
import Exercise2
import Exercise3
import Exercise4
import ExprT
import Exercise5
import Exercise6
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

  describe "skips" $ do
    it "should contain every nth element" $
      skips2 [True, False] `shouldBe` [[True, False], [False]]
    it "should contain every nth element" $
      skips2 "hello!" `shouldBe` ["hello!", "el!", "l!", "l", "o", "!"]

  describe "localMaxima" $ do
    it "should return any number greater than the numbers before or after" $
      localMaxima [2,9,5,6,1] `shouldBe` [9,6]

  describe "frequencies" $ do
    it "should the frequencies" $
      numberWithFrequencies [2,9,5,5,1] `shouldBe` [(1,1), (2,1), (5,2), (9,1)]

  describe "buildHistogram" $ do
    it "should the frequencies" $
      listOfFrequencies [2,9,5,5,1] `shouldBe` [0,1,1,0,0,2,0,0,0,1]

  describe "buildRow" $ do
    it "should the frequencies" $
      buildRow [0,1,1,0,0,2,0,0,0,1] 2 `shouldBe` "     *    "

  describe "histogram" $ do
    it "should the frequencies" $
      histogram [3,5] `shouldBe` "   * *    \n==========\n0123456789\n"

  describe "fun1'" $ do
    it "should be equal to fun1" $
      fun1' [12,5] `shouldBe` fun1 [12,5]

  describe "fun2'" $ do
    it "should be equal to fun2" $
      fun2' 12 `shouldBe` fun2 12

  describe "foldTree'" $ do
    it "should fold a balanced binary tree" $ do
      foldTree "A" `shouldBe` FNode 0 FLeaf 'A' FLeaf
      foldTree "AB" `shouldBe` FNode 1 FLeaf 'B' (FNode 0 FLeaf 'A' FLeaf)
      foldTree "ABC" `shouldBe` FNode 1 (FNode 0 FLeaf 'A' FLeaf) 'C' (FNode 0 FLeaf 'B' FLeaf)
      foldTree "ABCD" `shouldBe` FNode 2 (FNode 0 FLeaf 'B' FLeaf) 'D' (FNode 1 FLeaf 'C' (FNode 0 FLeaf 'A' FLeaf))
      foldTree "ABCDE" `shouldBe` FNode 2 (FNode 1 FLeaf 'C' (FNode 0 FLeaf 'A' FLeaf)) 'E' (FNode 1 FLeaf 'D' (FNode 0 FLeaf 'B' FLeaf))
      foldTree "ABCDEFGHIJ" `shouldBe` FNode 3 (FNode 2 (FNode 0 FLeaf 'C' FLeaf) 'H' (FNode 1 FLeaf 'F' (FNode 0 FLeaf 'B' FLeaf))) 'J' (FNode 2 (FNode 1 FLeaf 'E' (FNode 0 FLeaf 'A' FLeaf)) 'I' (FNode 1 FLeaf 'G' (FNode 0 FLeaf 'D' FLeaf)))

  describe "xor" $ do
    it "should return true if there are an odd number of true values" $ do
      xor [False, True, False] `shouldBe` True
      xor [False, True, False, False, True] `shouldBe` False

  describe "eval" $ do
    it "should calculate" $ do
      eval (Mul (Add (Lit 2) (Lit 3)) (Lit 4)) `shouldBe` 20

  describe "evalSt" $ do
    it "should calculate" $ do
      evalStr "2+3*" `shouldBe` Nothing
      evalStr "2+3*4" `shouldBe` Just 14

  describe "MinMax" $ do
    it "should do max on + and min on *" $ do
      add (lit 1) (lit 2) `shouldBe` MinMax 2

  describe "fib" $ do
    it "should return 0 for F0" $ do
      fib 0 `shouldBe` 0
    it "should return 1 for F1" $ do
      fib 1 `shouldBe` 1
    it "should return 1 for F2" $ do
      fib 2 `shouldBe` 1
    it "should return 8 for F6" $ do
      fib 4 `shouldBe` 3
    it "should return 14 for 377" $ do
      fib 30 `shouldBe` 832040
