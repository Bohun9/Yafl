module ANF.Builtins where

import qualified ANF.Syntax as A
import ANF.ToANF (toANFType)
import qualified Typed.Builtins

builtins :: [(A.Var, A.Type)]
builtins = map (\(n, t) -> (n, toANFType t)) Typed.Builtins.builtins
