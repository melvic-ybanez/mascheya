module Mascheya.ConstSpec where

import Data.List (isInfixOf)
import qualified Mascheya.Core.Runner as Runner
import System.IO (stderr, stdout)
import System.IO.Silently (hCapture)
import Test.Hspec (Spec, describe, it, shouldSatisfy)

spec :: Spec
spec = do
  let runCapture = hCapture [stdout, stderr] . Runner.runDefault
  describe "numeric value" $ do
    it "evaluates to itself" $ do
      (output, _) <- runCapture "100"
      output `shouldSatisfy` ("100" `isInfixOf`)

      (output, _) <- runCapture "200.56"
      output `shouldSatisfy` ("200.56" `isInfixOf`)

      (output, _) <- runCapture "'c'"
      output `shouldSatisfy` ("'c'" `isInfixOf`)