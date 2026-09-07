module Main where

import qualified Mascheya.ConstSpec as ConstSpec
import Test.Hspec (describe, hspec)

main :: IO ()
main = hspec $ do
  describe "Constants" ConstSpec.spec