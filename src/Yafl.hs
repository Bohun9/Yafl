module Yafl where

import ANF.Syntax as ANF
import ANF.ToANF as ToANF
import Core.ClosureConv as ClosureConv
import Core.Syntax as Core
import Desugared.Desugar as Desugar
import Prettyprinter
import Surface.Lexer as Lexer
import Surface.Parser as Parser
import Typed.Syntax as Typed
import Typed.Typecheck as Typecheck

pipeline :: String -> Core.Program
pipeline source =
  let tokens = Lexer.alexScanTokens source
      program = Parser.parseProgram tokens
      desugared = Desugar.desugar program
      typed = Typecheck.typecheck desugared
      anf = ToANF.toANF typed
      core = ClosureConv.closureConv anf
   in core

--
debugPipeline :: String -> IO ANF.Program
debugPipeline source = do
  let tokens = Lexer.alexScanTokens source
  print tokens
  print "Lexing done..."
  let program = Parser.parseProgram tokens
  print program
  print "Parsing done..."
  let desugared = Desugar.desugar program
  print desugared
  print "Desugar done..."
  let typed = Typecheck.typecheck desugared
  print typed
  print "Typecheck done..."
  let anf = ToANF.toANF typed
  print anf
  print "ANF done..."
  return anf
