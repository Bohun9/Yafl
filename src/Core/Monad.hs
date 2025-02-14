module Core.Monad where

import qualified ANF.Syntax as A
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import qualified Core.Syntax as C
import qualified Data.Map as Map

newtype EnvIndex = EnvIndex Integer
  deriving (Eq, Ord)

newtype Level = Level Int
  deriving (Eq, Ord)

data VarAccess
  = LocalAccess
  | EnvAccess
      { levelDiff :: Int,
        envIndex :: EnvIndex
      }
  | BuiltinFunAccess A.Type

data ClosureConvState = ClosureConvState
  { varEnvIndex :: Map.Map A.Tag EnvIndex,
    varAccess :: Map.Map A.Tag VarAccess,
    freshVarId :: Int,
    levelEnvSize :: Map.Map Level Integer
  }

data ClosureConvEnv = ClosureConvEnv
  { curEnvType :: C.Type
  }

data VarEntry
  = UserDefined Level A.Tag
  | BuiltinFun A.Type

data EscapeAnalEnv = EscapeAnalEnv
  { varTable :: Map.Map A.Var VarEntry,
    curLevel :: Level
  }

type ClosureConv = WriterT [C.TopLevelFun] (ReaderT ClosureConvEnv (State ClosureConvState))

type EscapeAnal = ReaderT EscapeAnalEnv (State ClosureConvState)

freshVar :: String -> ClosureConv (C.Var, C.Value)
freshVar hint = do
  id <- gets freshVarId
  modify (\s -> s {freshVarId = id + 1})
  let x = "_" ++ hint ++ show id
  return $ (x, C.VLocalVar x)
