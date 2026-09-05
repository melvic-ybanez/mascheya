{-# LANGUAGE RankNTypes #-}

module Mascheya.Core.Runner where

import Mascheya.Core.Eval.Value (VEnv, Value (DefNelVal), Def (Def))
import qualified Mascheya.Core.Eval as Eval
import Control.Monad.Reader (ReaderT(runReaderT))
import qualified Mascheya.Core.Parser as Parser
import qualified Mascheya.Core.Translate as Translate
import Mascheya.Core.Display (Display(display))
import qualified Mascheya.Core.Result as Result
import Control.Monad.Except (runExceptT, ExceptT (ExceptT))
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.IORef (readIORef)
import qualified Mascheya.Core.Predef.Printer as Printer

run :: String -> VEnv -> IO VEnv
run input env = do
  resultEither <- result
  case resultEither of
    Left error' -> Printer.putErrorLn (display error') >> pure env
    Right (val, newEnv) -> Printer.printLn newEnv val >> pure newEnv
  where
    result = runExceptT $ do
      sourceProg <- Result.liftT $ Parser.parse Parser.program input
      coreProg <- Result.liftT $ Translate.toCoreProg sourceProg
      val <- runReaderT (Eval.eval coreProg) env
      newEnv <- ExceptT $ case val of
        DefNelVal ((Def newEnvRef) :| _) -> Result.succeed <$> readIORef newEnvRef
        _ -> return $ Result.succeed env
      ExceptT $ return $ Result.succeed (val, newEnv)
  