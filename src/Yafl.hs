module Yafl where

import ANF.Syntax as ANF
import ANF.ToANF as ToANF
import Desugared.Desugar as Desugar
import Prettyprinter
import Surface.Lexer as Lexer
import Surface.Parser as Parser
import Typed.Syntax as Typed
import Typed.Typecheck as Typecheck

pipeline :: String -> ANF.Program
pipeline source =
  let tokens = Lexer.alexScanTokens source
      program = Parser.parseProgram tokens
      desugared = Desugar.desugar program
      typed = Typecheck.typecheck desugared
      anf = ToANF.toANF typed
   in anf
