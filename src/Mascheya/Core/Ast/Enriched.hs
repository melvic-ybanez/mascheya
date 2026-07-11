module Mascheya.Core.Ast.Enriched where
  
import Mascheya.Core.Ast.Core (CExpr, CVar, CConst)
import Data.List.NonEmpty (NonEmpty)

data EExpr = ECoreExpr CExpr | EDataExpr EData  | EData deriving Show

data EData = EProductData EProduct | ESumData ESum deriving Show

data EProduct = EProduct EConstructor [EType] deriving Show

data ESum = ESum (NonEmpty EProduct) deriving Show

data EConstructor = EConstructor String deriving Show

data EType = EType String deriving Show

data EPattern = EVarPat CVar | EConstPat CConst 
  | EConstructorPat EConstructor [EPattern] deriving Show