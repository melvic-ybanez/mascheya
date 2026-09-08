module Mascheya.Core.DefSpec where

import Mascheya.TestRunner (runErrorContains, runSuccessEquals)
import Test.Hspec (Spec, describe, it)

spec :: Spec
spec = do
  it "should not print anything" $ do
    runSuccessEquals "c = '\^A'" ""
  describe "Undefined variables" $ do
    it "should throw an error" $ do
      runErrorContains "c" "Undefined variable"