module Mascheya.Core.ConstSpec where

import Mascheya.TestRunner (runSuccessEquals)
import Test.Hspec (Spec, describe, it)

spec :: Spec
spec = do
  describe "numeric value" $ do
    it "evaluates to itself" $ do
      runSuccessEquals "100" "100"
      runSuccessEquals "200.56" "200.56"

  describe "character" $ do
    it "is wrapped in single quotes" $ do
      runSuccessEquals "'c'" "'c'"

    it "supports control codes" $ do
      runSuccessEquals "'\^A'" "'\\SOH'"