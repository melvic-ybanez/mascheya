module Main where

import Test.Tasty
import Test.Tasty.QuickCheck (testProperty)
import Mascheya.Core.Ast.CoreSpec (propOccursFreeInSelf, propOccursFreeInOtherVars)

main :: IO ()
main = defaultMain $ testGroup "All Occurs Free Tests" [
        testProperty "x occurs free in x" propOccursFreeInSelf,
        testProperty "x does not occur free in other vars" propOccursFreeInOtherVars
    ]