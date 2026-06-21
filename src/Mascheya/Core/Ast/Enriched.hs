module Mascheya.Core.Ast.Enriched where

import Mascheya.Core.Ast.Core (CExpr, CVar)

data EExpr = ECoreExpr CExpr | ELetExpr ELet deriving Show

data ELet = ELet CVar EExpr EExpr deriving Show