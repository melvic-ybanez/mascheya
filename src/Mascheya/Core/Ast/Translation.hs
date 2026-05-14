module Mascheya.Core.Ast.Translation where
import qualified Mascheya.Core.Ast.Source as S
import qualified Mascheya.Core.Ast.Core as C
import Mascheya.Core.Parsers.Types (Step(value))

translation :: S.Expr -> C.Expr
translation (S.Literal (S.Int value)) =  C.Constant $ C.Number $ fromIntegral value
