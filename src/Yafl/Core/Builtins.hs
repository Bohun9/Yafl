module Yafl.Core.Builtins where

import qualified Yafl.ANF.Builtins as ANF.Builtins
import qualified Yafl.ANF.Syntax as A
import Yafl.Core.ClosureConv (convertType, voidPointer)
import qualified Yafl.Core.Syntax as C

builtinFuns :: [(C.Var, [C.Type], C.Type)]
builtinFuns =
  map
    ( \(n, A.TArrow t1 t2) ->
        (n, [voidPointer, convertType t1], convertType t2)
    )
    ANF.Builtins.builtinFuns
