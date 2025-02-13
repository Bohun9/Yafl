module Main where

import qualified Data.Text.Lazy as TL
import qualified LLVM.Pretty
import System.Environment
import qualified Yafl as Yafl

main :: IO ()
main = do
  args <- getArgs
  let filePath = head args
  source <- readFile filePath
  -- let result = Yafl.pipeline source
  -- print result
  Yafl.debugPipeline source

  let llvmMod = Yafl.pipelineLLVM source
  let llvmSource = TL.unpack $ LLVM.Pretty.ppllvm llvmMod
  writeFile "output.ll" llvmSource
