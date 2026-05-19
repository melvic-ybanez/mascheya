module Main where

import Test.Tasty
import Test.Tasty.QuickCheck (testProperty)
import Mascheya.Core.Ast.CoreSpec (propOccursFreeInSelf)

main :: IO ()
main = defaultMain $ testGroup "All Expr Tests" [
        testProperty "x occurs free in x" propOccursFreeInSelf
    ]