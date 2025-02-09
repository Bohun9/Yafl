module Yafl where

import Desugared.Desugar as Desugar
import Prettyprinter
import Surface.Lexer as Lexer
import Surface.Parser as Parser
import Typed.Syntax as Typed
import Typed.Typecheck as Typecheck

pipeline :: String -> Typed.Program
pipeline source =
  let tokens = Lexer.alexScanTokens source
      program = Parser.parseProgram tokens
      desugared = Desugar.desugar program
      typed = Typecheck.typecheck desugared
   in typed
