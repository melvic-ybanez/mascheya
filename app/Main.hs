module Main where

import Mascheya.Core.Repl

main :: IO ()
main = repl State { lineMode = Single }
