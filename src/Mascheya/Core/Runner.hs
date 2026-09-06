{-# LANGUAGE RankNTypes #-}

module Mascheya.Core.Runner where

import Control.Monad.Except (ExceptT (ExceptT), runExceptT)
import Control.Monad.Reader (ReaderT (runReaderT))
import Data.IORef (readIORef)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Mascheya.Core.Display (Display (display))
import qualified Mascheya.Core.Eval as Eval
import Mascheya.Core.Eval.Value (Def (Def), VEnv, Value (DefNelVal))
import qualified Mascheya.Core.Parser as Parser
import qualified Mascheya.Core.Predef.Printer as Printer
import qualified Mascheya.Core.Result as Result
import qualified Mascheya.Core.Translate as Translate

run :: String -> VEnv -> IO VEnv
run input env = do
  result <- runExceptT $ do
    sourceProg <- Result.liftT $ Parser.parse Parser.program input
    coreProg <- Result.liftT $ Translate.toCoreProg sourceProg
    val <- runReaderT (Eval.eval coreProg) env
    newEnv <- ExceptT $ case val of
      DefNelVal ((Def newEnvRef) :| _) -> Result.succeed <$> readIORef newEnvRef
      _ -> return $ Result.succeed env
    ExceptT $ return $ Result.succeed (val, newEnv)
  case result of
    Left error' -> Printer.putErrorLn (display error') >> pure env
    Right (val, newEnv) -> Printer.printLn newEnv val >> pure newEnv
