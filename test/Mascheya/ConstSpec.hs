module Mascheya.ConstSpec where

import Mascheya.Common (runCheckEqual)
import Test.Hspec (Spec, describe, it)

spec :: Spec
spec = do
  describe "numeric value" $ do
    it "evaluates to itself" $ do
      runCheckEqual "100" "100"
      runCheckEqual "200.56" "200.56"

  describe "character" $ do
    it "is wrapped in single quotes" $ do
      runCheckEqual "'c'" "'c'"

    it "supports control codes" $ do
      runCheckEqual "'\^A'" "'\\SOH'"