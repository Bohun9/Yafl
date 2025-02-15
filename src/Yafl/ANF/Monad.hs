module Yafl.ANF.Monad where

import Control.Monad.Reader
import Control.Monad.State
import qualified Data.Map as Map
import qualified Yafl.ANF.Syntax as A
import qualified Yafl.Typed.Syntax as T

data ANFState = ANFState
  { freshVarId :: Int,
    freshTagId :: Int
  }

newtype ANFEnv = ANFEnv
  { typeInfo :: T.TypeInfo
  }

type ANFTransform = StateT ANFState (Reader ANFEnv)

freshVar :: String -> ANFTransform A.Var
freshVar hint = do
  id <- gets freshVarId
  modify (\s -> s {freshVarId = id + 1})
  return $ "_" ++ hint ++ show id

freshTag :: ANFTransform A.Tag
freshTag = do
  id <- gets freshTagId
  modify (\s -> s {freshTagId = id + 1})
  return $ A.Tag id

data AdtRepr
  = EnumRepr
  | RecordRepr

adtRepr :: T.Var -> ANFTransform AdtRepr
adtRepr adt = do
  adts <- reader (T.adts . typeInfo)
  case Map.lookup adt adts of
    Just T.AdtInfo {T.isEnum = isEnum} ->
      if isEnum
        then return EnumRepr
        else return RecordRepr
    Nothing -> error "internal error"

ctorRepr :: T.Var -> ANFTransform AdtRepr
ctorRepr ctor = do
  ctors <- reader (T.ctors . typeInfo)
  case Map.lookup ctor ctors of
    Just T.CtorInfo {T.adtName = adt} -> adtRepr adt
    Nothing -> error "internal error"

ctorTagId :: T.Var -> ANFTransform Int
ctorTagId ctor = do
  ctors <- reader (T.ctors . typeInfo)
  case Map.lookup ctor ctors of
    Just info -> return $ T.tagId info
    Nothing -> error "internal error"

ctorFields :: T.Var -> ANFTransform [T.Type]
ctorFields ctor = do
  ctorsInfo <- reader (T.ctors . typeInfo)
  case Map.lookup ctor ctorsInfo of
    Just info -> return $ T.fields info
    Nothing -> error "internal error"

makeVar :: A.Var -> A.Type -> ANFTransform A.VarInfo
makeVar x t = do
  tag <- freshTag
  return $ A.VarInfo {A.name = x, A.tag = tag, A.typ = t}

makeUseVar :: A.Var -> A.Type -> ANFTransform A.Value
makeUseVar x t = A.VVar <$> makeVar x t

freshUseVar :: String -> A.Type -> ANFTransform (A.Var, A.Value)
freshUseVar h t = do
  xName <- freshVar h
  xValue <- makeUseVar xName t
  return $ (xName, xValue)
