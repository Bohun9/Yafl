module Main where

import Data.Maybe (fromMaybe)
import qualified Data.Text.Lazy as TL
import qualified LLVM.Pretty
import Options.Applicative
import Prettyprinter
import Prettyprinter.Render.Text
import System.Directory (removeFile)
import System.Process (callProcess)
import qualified Yafl as Yafl

data Options = Options
  { srcPath :: String,
    dstPath :: Maybe String,
    dumpCore :: Bool,
    emitLLVM :: Bool,
    optimize :: Bool
  }

optionsParser :: Parser Options
optionsParser =
  Options
    <$> argument
      str
      ( metavar "SRC"
          <> help "Path to the source file"
      )
    <*> optional
      ( strOption
          ( short 'o'
              <> metavar "OUTPUT"
              <> help "Path to the output file (default: a.out or a.ll)"
          )
      )
    <*> switch
      ( long "dump-core"
          <> help "Dump core intermediate representation"
      )
    <*> switch
      ( long "emit-llvm"
          <> help "Emit LLVM IR instead of executable"
      )
    <*> switch
      ( short 'O'
          <> help "Enable LLVM optimizations (passes -O2 to opt-14)"
      )

optionsInfo :: ParserInfo Options
optionsInfo =
  info
    (optionsParser <**> helper)
    ( fullDesc
        <> progDesc "Compile a source file"
        <> header "yafl-compiler - A simple functional language compiler"
    )

main :: IO ()
main = do
  opts <- execParser optionsInfo
  programSource <- readFile (srcPath opts)
  if dumpCore opts
    then do
      let core = Yafl.corePipeline programSource
      putDoc (pretty core)
      putStrLn ""
    else do
      let llvmMod = Yafl.llvmPipeline programSource
          llvmSource = TL.unpack $ LLVM.Pretty.ppllvm llvmMod
          defaultOutput = if emitLLVM opts then "a.ll" else "a.out"
          outputPath = fromMaybe defaultOutput $ dstPath opts
      if emitLLVM opts
        then do
          writeFile outputPath llvmSource
          if optimize opts then callProcess "opt-14" ["-O2", "-S", outputPath, "-o", outputPath] else return ()
        else do
          let tmpFile = "tmp.ll"
          writeFile tmpFile llvmSource
          if optimize opts then callProcess "opt-14" ["-O2", "-S", tmpFile, "-o", tmpFile] else return ()
          callProcess "clang" [tmpFile, "runtime.c", "-o", outputPath]
          removeFile tmpFile
