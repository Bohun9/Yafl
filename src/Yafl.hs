module Yafl
  ( corePipeline,
    llvmPipeline,
  )
where

import qualified ANF.ToANF
import qualified CodeGen.LLVM
import qualified Core.ClosureConv
import qualified Core.Pretty
import qualified Core.Syntax
import qualified Desugared.Desugar
import qualified LLVM.AST
import qualified Surface.Lexer
import qualified Surface.Parser
import qualified Typed.Typecheck

corePipeline :: String -> Core.Syntax.Program
corePipeline =
  Core.ClosureConv.closureConv
    . ANF.ToANF.toANF
    . Typed.Typecheck.typecheck
    . Desugared.Desugar.desugar
    . Surface.Parser.parseProgram
    . Surface.Lexer.alexScanTokens

llvmPipeline :: String -> LLVM.AST.Module
llvmPipeline = CodeGen.LLVM.codeGen . corePipeline
