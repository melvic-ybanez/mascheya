module Mascheya.ConstSpec where

import qualified Mascheya.Core.Runner as Runner
import System.IO (stderr, stdout)
import System.IO.Silently (hCapture)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  let runCapture = hCapture [stdout, stderr] . Runner.runDefault putStr

  describe "numeric value" $ do
    it "evaluates to itself" $ do
      (output, _) <- runCapture "100"
      output `shouldBe` "100"

      (output, _) <- runCapture "200.56"
      output `shouldBe` "200.56"

  describe "character" $ do
    it "evaluates to itself if simple" $ do
      (output, _) <- runCapture "'c'"
      output `shouldBe` "'c'"

    it "supports control codes" $ do
      (output, _) <- runCapture "'\^A'"
      output `shouldBe` "'\\SOH'"