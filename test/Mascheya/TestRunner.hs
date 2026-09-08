module Mascheya.TestRunner where

import Data.List (isInfixOf)
import Mascheya.Core.Eval.Value (VEnv)
import qualified Mascheya.Core.Parser as Parser
import qualified Mascheya.Core.Runner as Runner
import System.IO (stderr)
import System.IO.Silently (capture, hCapture)
import Test.Hspec (Expectation, shouldBe, shouldSatisfy)

type RunCheck = String -> String -> IO ()

runCapture :: String -> IO (String, String, VEnv)
runCapture input = do
  (stdOut, (errOut, env)) <- capture $ hCapture [stderr] $ Runner.runDefault putStr input
  return (stdOut, errOut, env)

runSuccessEquals :: RunCheck
runSuccessEquals =
  runCheckWith
    ( \output err expected -> do
        err `shouldBe` ""
        output `shouldBe` expected
    )

runErrorContains :: RunCheck
runErrorContains =
  runCheckWith
    ( \output err expected -> do
        output `shouldBe` ""
        err `shouldSatisfy` (expected `isInfixOf`)
    )

runCheckWith :: (String -> String -> String -> Expectation) -> RunCheck
runCheckWith check input expected = do
  (stdOut, errOut, _) <- runCapture $ Parser.trim input
  check stdOut errOut expected