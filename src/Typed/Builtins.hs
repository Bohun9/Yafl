module Typed.Builtins where

import qualified Desugared.Syntax as D
import qualified Typed.Syntax as T

builtins :: [(D.Var, T.Type)]
builtins =
  [ ("print_int", T.TArrow T.TInt T.TInt),
    ("read_int", T.TArrow T.TInt T.TInt),
    ("print_newline", T.TArrow T.TInt T.TInt),
    ("print_space", T.TArrow T.TInt T.TInt)
  ]
