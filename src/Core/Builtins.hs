module Core.Builtins where

import qualified ANF.Builtins
import qualified ANF.Syntax as A
import Core.ClosureConv (convertType, voidPointer)
import qualified Core.Syntax as C

builtinFuns :: [(C.Var, [C.Type], C.Type)]
builtinFuns =
  map
    ( \(n, A.TArrow t1 t2) ->
        (n, [voidPointer, convertType t1], convertType t2)
    )
    ANF.Builtins.builtinFuns
