module Main where

import Test.Tasty
import Test.Tasty.QuickCheck (testProperty)
import Mascheya.Core.Ast.CoreSpec (propOccursFreeInSelf, propOccursFreeInOtherVars, propOccursFreeInApp)

main :: IO ()
main = defaultMain $ testGroup "All Occurs Free Tests" [
        testProperty "x occurs free in x" propOccursFreeInSelf,
        testProperty "x does not occur free in other vars" propOccursFreeInOtherVars,
        testProperty "x occurs free in application" propOccursFreeInApp
    ]