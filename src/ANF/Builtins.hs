module ANF.Builtins where

import qualified ANF.Syntax as A
import ANF.ToANF (toANFTypePure)
import qualified Typed.Builtins

builtinFuns :: [(A.Var, A.Type)]
builtinFuns = map (\(n, t) -> (n, toANFTypePure t)) Typed.Builtins.builtinFuns
