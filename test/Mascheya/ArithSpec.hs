module Mascheya.ArithSpec where

import Mascheya.Common (runCheckEqual)
import Test.Hspec (Spec, describe, it)

spec :: Spec
spec = do
  describe "infix operator" $ do
    it "applies to numerical operands of the same type" $ do
      runCheckEqual "456.67f + 56.7f" "513.37"