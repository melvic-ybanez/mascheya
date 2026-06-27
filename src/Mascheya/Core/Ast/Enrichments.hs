module Mascheya.Core.Ast.Enrichments where

import Mascheya.Core.Ast.Core (CExpr, CVar)

data EExpr = ELetExpr ELet deriving Show

data ELet = ELet CVar CExpr CExpr deriving Show