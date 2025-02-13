module Yafl where

import ANF.Syntax as ANF
import qualified ANF.ToANF
import qualified CodeGen.LLVM
import qualified Core.ClosureConv
import qualified Core.Pretty
import Core.Syntax as Core
import qualified Desugared.Desugar
import qualified LLVM.AST
import Prettyprinter
import Prettyprinter.Render.Text
import qualified Surface.Lexer
import qualified Surface.Parser
import qualified Typed.Typecheck

pipeline :: String -> Core.Program
pipeline source =
  let tokens = Surface.Lexer.alexScanTokens source
      program = Surface.Parser.parseProgram tokens
      desugared = Desugared.Desugar.desugar program
      typed = Typed.Typecheck.typecheck desugared
      anf = ANF.ToANF.toANF typed
      core = Core.ClosureConv.closureConv anf
   in core

pipelineLLVM :: String -> LLVM.AST.Module
pipelineLLVM source =
  let tokens = Surface.Lexer.alexScanTokens source
      program = Surface.Parser.parseProgram tokens
      desugared = Desugared.Desugar.desugar program
      typed = Typed.Typecheck.typecheck desugared
      anf = ANF.ToANF.toANF typed
      core = Core.ClosureConv.closureConv anf
      llvm = CodeGen.LLVM.codeGen core
   in llvm

--
debugPipeline :: String -> IO ()
debugPipeline source = do
  let tokens = Surface.Lexer.alexScanTokens source
  -- print tokens
  -- print "Lexing done..."
  let program = Surface.Parser.parseProgram tokens
  -- print program
  -- print "Parsing done..."
  let desugared = Desugared.Desugar.desugar program
  -- print desugared
  -- print "Desugar done..."
  let typed = Typed.Typecheck.typecheck desugared
  -- print typed
  -- print "Typecheck done..."
  let anf = ANF.ToANF.toANF typed
  -- print anf
  -- print "ANF done..."
  let core = Core.ClosureConv.closureConv anf
  -- print core
  -- print "Closure Conversion done..."
  -- print "------------------"
  putDoc (pretty core)
  putStrLn ""
