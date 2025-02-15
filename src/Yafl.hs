module Yafl
  ( corePipeline,
    llvmPipeline,
  )
where

import qualified LLVM.AST
import qualified Yafl.ANF.ToANF as ANF
import qualified Yafl.CodeGen.LLVM as CG
import qualified Yafl.Core.ClosureConv as CC
import qualified Yafl.Core.Pretty
import qualified Yafl.Core.Syntax as Core
import qualified Yafl.Desugared.Desugar as Desugar
import qualified Yafl.Surface.Lexer as Lexer
import qualified Yafl.Surface.Parser as Parser
import qualified Yafl.Typed.Typecheck as Typecheck

corePipeline :: String -> Core.Program
corePipeline =
  CC.closureConv
    . ANF.toANF
    . Typecheck.typecheck
    . Desugar.desugar
    . Parser.parseProgram
    . Lexer.alexScanTokens

llvmPipeline :: String -> LLVM.AST.Module
llvmPipeline = CG.codeGen . corePipeline
