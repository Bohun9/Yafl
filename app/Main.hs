module Main where

import System.Environment
import qualified Yafl as Yafl

main :: IO ()
main = do
  args <- getArgs
  let filePath = head args
  source <- readFile filePath
  let result = Yafl.pipeline source
  print result
