module Yafl.Core.Monad where

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import qualified Data.Map as Map
import qualified Yafl.ANF.Syntax as A
import qualified Yafl.Core.Syntax as C

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
  let x = "_" ++ hint ++ "_" ++ show id
  return $ (x, C.VLocalVar x)

extendVarTable :: A.Var -> A.Tag -> EscapeAnal a -> EscapeAnal a
extendVarTable x tag m = do
  level <- reader curLevel
  local (\r -> r {varTable = Map.insert x (UserDefined level tag) (varTable r)}) m

lookupVarTable :: A.Var -> EscapeAnal VarEntry
lookupVarTable x = do
  varTable <- reader varTable
  case Map.lookup x varTable of
    Just r -> return r
    Nothing -> error "internal error"

insertVarAccess :: A.Tag -> VarAccess -> EscapeAnal ()
insertVarAccess tag access =
  modify (\s -> s {varAccess = Map.insert tag access (varAccess s)})
