module ANF.ToANF where

import qualified ANF.Syntax as A
import Control.Monad.Reader
import Control.Monad.State
import qualified Data.Map as Map
import qualified Typed.Syntax as T

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

ctorTagId :: T.Var -> ANFTransform Int
ctorTagId ctor = do
  ctorsInfo <- reader (T.ctors . typeInfo)
  case Map.lookup ctor ctorsInfo of
    Just info -> return $ T.tagId info
    Nothing -> error "internal error"

ctorFields :: T.Var -> ANFTransform [T.Type]
ctorFields ctor = do
  ctorsInfo <- reader (T.ctors . typeInfo)
  case Map.lookup ctor ctorsInfo of
    Just info -> return $ T.fields info
    Nothing -> error "internal error"

toANFType :: T.Type -> A.Type
toANFType T.TInt = A.TInt
toANFType (T.TArrow t1 t2) = A.TArrow (toANFType t1) (toANFType t2)
toANFType (T.TAdt _) = A.TStructPointer [A.TInt]

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

buildExpr :: [(A.Var, A.Type, A.Expr)] -> A.Expr -> ANFTransform A.Expr
buildExpr defs e =
  foldM
    ( \acc (x, t, e) -> do
        v <- makeVar x t
        return $ A.ELet v e acc
    )
    e
    (reverse defs)

toANFClause :: A.Value -> T.Clause -> ANFTransform (Integer, A.Expr)
toANFClause v (T.Clause (T.PCtor c xs) e) = do
  fields <- ctorFields c
  tag <- ctorTagId c
  w <- freshVar "ctor"
  let transFields = map toANFType fields
      ctorType = A.TStructPointer $ A.TInt : transFields
  es <-
    mapM
      (\i -> A.EFetch <$> makeUseVar w ctorType <*> return i)
      [1 .. toInteger $ length fields]
  e' <- toANFExpr e
  body <-
    buildExpr
      ( (w, ctorType, A.ECast ctorType v)
          : zip3 xs transFields es
      )
      e'
  return (toInteger tag, body)

toANFExpr :: T.Expr -> ANFTransform A.Expr
toANFExpr T.Annot {T.value = e, T.typ = t} = toANFExpr' e (toANFType t)

toANFExpr' :: T.Expr' -> A.Type -> ANFTransform A.Expr
toANFExpr' e t =
  case e of
    T.EInt n -> return $ A.EValue (A.VInt n)
    T.EVar x -> A.EValue <$> makeUseVar x t
    T.EBinop op e1 e2 ->
      toANFExprName
        e1
        ( \v1 ->
            toANFExprName
              e2
              ( \v2 ->
                  return $ A.EBinop op v1 v2
              )
        )
    T.EApp e1 e2 ->
      toANFExprName
        e1
        ( \v1 ->
            toANFExprName
              e2
              ( \v2 ->
                  return $ A.EApp v1 v2
              )
        )
    T.ELet x e1 e2 -> do
      e1' <- toANFExpr e1
      e2' <- toANFExpr e2
      let t = (toANFType . T.typ) e1
      buildExpr [(x, t, e1')] e2'
    T.EFun f x t1 t2 e1 e2 -> do
      e1' <- toANFExpr e1
      e2' <- toANFExpr e2
      let t1' = toANFType t1
          t2' = toANFType t2
          t' = A.TArrow t1' t2'
      f' <- makeVar f t'
      x' <- makeVar x t1'
      buildExpr [(f, t', A.EValue (A.VFun f' x' e1'))] e2'
    T.ECtor c es -> do
      let fieldTypes = A.TInt : map (toANFType . T.typ) es
      let rType = A.TStructPointer fieldTypes
      (r, r') <- freshUseVar "r" rType
      tagId <- ctorTagId c
      toANFExprNames
        es
        ( \vs ->
            buildExpr
              [(r, rType, A.EMakeRecord fieldTypes $ A.VInt tagId : vs)]
              (A.ECast (A.TStructPointer [A.TInt]) r')
        )
    T.ECase e clauses ->
      toANFExprName
        e
        ( \v -> do
            (t, t') <- freshUseVar "tag" A.TInt
            es <- mapM (toANFClause v) clauses
            buildExpr
              [(t, A.TInt, A.EFetch v 0)]
              (A.ESwitch t' es)
        )
    T.EPatternMatchingSeq e1 e2 -> A.EPatternMatchingSeq <$> toANFExpr e1 <*> toANFExpr e2
    T.EPatternMatchingError -> return A.EPatternMatchingError

toANFExprName :: T.Expr -> (A.Value -> ANFTransform A.Expr) -> ANFTransform A.Expr
toANFExprName e k = do
  e1 <- toANFExpr e
  case e1 of
    A.EValue v -> k v
    _ -> do
      let t = toANFType $ T.typ e
      (a, a') <- freshUseVar "a" t
      e2 <- k a'
      buildExpr [(a, t, e1)] e2

toANFExprNames :: [T.Expr] -> ([A.Value] -> ANFTransform A.Expr) -> ANFTransform A.Expr
toANFExprNames [] k = k []
toANFExprNames (e : es) k = toANFExprName e (\v -> toANFExprNames es (\vs -> k $ v : vs))

toANF :: T.Program -> A.Program
toANF (T.Program typeInfo e) =
  let initState =
        ANFState
          { freshVarId = 0,
            freshTagId = 0
          }
      initEnv =
        ANFEnv
          { typeInfo = typeInfo
          }
   in A.Program $ runReader (evalStateT (toANFExpr e) initState) initEnv
