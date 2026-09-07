module Mascheya.Common where

import Mascheya.Core.Eval.Value (VEnv)
import qualified Mascheya.Core.Runner as Runner
import System.IO (stderr, stdout)
import System.IO.Silently (hCapture)
import Test.Hspec (shouldBe)

runCapture :: String -> IO (String, VEnv)
runCapture = hCapture [stdout, stderr] . Runner.runDefault putStr

runCheckEqual :: String -> String -> IO ()
runCheckEqual input expected = do
  (output, _) <- runCapture input
  output `shouldBe` expected