module Mascheya.Core.LetSpec where

import Mascheya.TestRunner (runSuccessEquals)
import Test.Hspec (Spec, describe, it)

spec :: Spec
spec = do
  describe "bounded variable" $ do
    it "should be accessible inside the let-expression body" $ do
      runSuccessEquals "let x = 10 in x * 29" "290"
      runSuccessEquals "let x = 10 in x % 3" "1"
  describe "nested expressions" $ do
    it "inner let-expression can access outer bounded variables" $ do
      runSuccessEquals "let a = 8 + 9 in let b = 5 * 6 in a < b" "True"
  it "supports multiple definitions" $ do
    runSuccessEquals (unlines ["let x = 10;", "y = 20;", "z = 30", "in x + y * z"]) "610"