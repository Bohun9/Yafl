module Yafl.Typed.Builtins where

import qualified Yafl.Desugared.Syntax as D
import qualified Yafl.Typed.Syntax as T

builtinFuns :: [(D.Var, T.Type)]
builtinFuns =
  [ ("print_int", T.TArrow T.TInt (T.TAdt "unit")),
    ("read_int", T.TArrow (T.TAdt "unit") T.TInt),
    ("print_newline", T.TArrow (T.TAdt "unit") (T.TAdt "unit")),
    ("print_space", T.TArrow (T.TAdt "unit") (T.TAdt "unit"))
  ]

builtinAdts :: [(T.Var, [(T.Var, [T.Type])])]
builtinAdts =
  [("unit", [("Unit", [])])]
