module Mascheya.Core.Repl where

import System.IO (hFlush, stdout)
import System.Exit (die)

repl :: IO ()
repl = do
    putStr "> "
    hFlush stdout
    input <- getLine
    if input == ":q" then die "Bye!"
    else do
        putStrLn input
        putStrLn ""
        repl