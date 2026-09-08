module Mascheya.Core.ArithSpec where

import Mascheya.TestRunner (runSuccessEquals)
import Test.Hspec (Spec, describe, it)

spec :: Spec
spec = do
  describe "infix operator" $ do
    it "applies to numerical operands of the same type" $ do
      runSuccessEquals "456.67f + 56.7f" "513.37"