module Yafl where

import           Desugared.Desugar
import           Prettyprinter
import           Surface.Lexer
import           Surface.Parser

pipeline :: String -> IO ()
pipeline source = do
  let tokens = alexScanTokens source
  let program = parseProgram tokens
  let desugared = desugar program
  print desugared
