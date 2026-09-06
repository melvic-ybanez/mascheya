module Mascheya.Core.Predef.Printer where

import Control.Monad.Except (runExceptT)
import Control.Monad.Reader (ReaderT (runReaderT))
import Data.List (intersperse)
import Mascheya.Core.Display (Display (display))
import Mascheya.Core.Eval (evalExpr, force)
import Mascheya.Core.Eval.Value
import qualified Mascheya.Core.Lexemes as Lexemes
import Mascheya.Core.Result (Result)
import qualified Mascheya.Core.Result as Result
import Prelude hiding (print)

type Print a = a -> IO ()

print :: VEnv -> Print Value
print = printWith putSuccess

printLn :: VEnv -> Print Value
printLn = printWith putSuccessLn

-- | Eagerly prints the value using the given printing function.
-- Right now, we don't have a `show` function yet, so let's just print the value directly,
-- forcing every thunks to be evaluated.
-- TODO: Implement `show` and define this function in terms of it.
printWith :: Print String -> VEnv -> Print Value
printWith print' env (ThunkVal thunk) = do
  result <- runExceptT $ force thunk
  printErrorOr (printWith print' env) result
printWith print' _ (ClosureVal _) = print' "<function>"
printWith print' _ (ConstVal const') = print' $ case const' of
  IntVal i -> display i
  FloatVal f -> display f
  DoubleVal d -> display d
  CharVal c -> display c
  BoolVal b -> display b
  Unit -> Lexemes.unit
printWith print' _ (ListVal Nil) = print' "Nil"
printWith print' env (ListVal (Cons hTh tTh)) = do
  result <- runExceptT $ do
    h <- force hTh
    t <- force tTh
    _ <- pure $ print' "Cons" >> print' [Lexemes.space]
    _ <- pure $ printWith print' env h >> print' [Lexemes.space]
    pure $ case t of
      ListVal _ -> do
        print' [Lexemes.leftParen]
        printWith print' env t
        print' [Lexemes.rightParen]
      _ -> putErrorLn "Tail is not a list"
  printErrorOr (const $ pure ()) result
printWith print' _ (DefNelVal _) = print' Lexemes.unit
printWith print' env (ProdVal (Product name comps)) = do
  print' name
  result <- runExceptT $ sequence $ flip runReaderT env . evalExpr <$> comps
  printErrorOr
    ( \values -> case values of
        [] -> pure ()
        xs -> sequence_ $ intersperse (print' [Lexemes.comma]) $ printWith print' env <$> xs
    )
    result
printWith _ _ (MatchFailVal (MatchFail at)) = putErrorLn $ display $ Result.matchError at
printWith _ _ (BottomVal (Bottom at)) = putErrorLn $ display $ Result.undefinedCalled at

printErrorOr :: (a -> IO ()) -> Result a -> IO ()
printErrorOr _ (Left errs) = putErrorLn $ display errs
printErrorOr f (Right a) = f a

putInfoLn :: Print String
putInfoLn = putStrLn . brightCyanStr

putErrorLn :: Print String
putErrorLn = putStrLn . brightRedStr

putSuccess :: Print String
putSuccess = putStr . brightGreenStr

putSuccessLn :: Print String
putSuccessLn = (>> putStrLn "") . putSuccess

-- | Color a string using ANSI escape codes
coloredStr :: Int -> String -> String
coloredStr i msg = "\ESC[" ++ show i ++ "m" ++ msg ++ "\ESC[0m"

brightCyanStr :: String -> String
brightCyanStr = coloredStr 96

brightRedStr :: String -> String
brightRedStr = coloredStr 91

brightGreenStr :: String -> String
brightGreenStr = coloredStr 92