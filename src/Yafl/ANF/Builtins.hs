module Yafl.ANF.Builtins where

import qualified Yafl.ANF.Syntax as A
import Yafl.ANF.ToANF (toANFTypePure)
import qualified Yafl.Typed.Builtins as Typed.Builtins

builtinFuns :: [(A.Var, A.Type)]
builtinFuns = map (\(n, t) -> (n, toANFTypePure t)) Typed.Builtins.builtinFuns
