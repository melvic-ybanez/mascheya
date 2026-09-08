module Main where

import qualified Mascheya.Core.ArithSpec as ArithSpec
import qualified Mascheya.Core.ConstSpec as ConstSpec
import qualified Mascheya.Core.DefSpec as DefSpec
import qualified Mascheya.Core.LetSpec as LetSpec
import Test.Hspec (describe, hspec)

main :: IO ()
main = hspec $ do
  describe "Constants" ConstSpec.spec
  describe "Arithmetic" ArithSpec.spec
  describe "Let-expressions" LetSpec.spec
  describe "Definitions" DefSpec.spec