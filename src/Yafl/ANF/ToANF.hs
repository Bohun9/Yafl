module Yafl.ANF.ToANF where

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

toANFType :: T.Type -> ANFTransform A.Type
toANFType T.TInt = return A.TInt
toANFType T.TBool = return A.TBool
toANFType (T.TArrow t1 t2) = A.TArrow <$> toANFType t1 <*> toANFType t2
toANFType (T.TAdt adt) = do
  repr <- adtRepr adt
  return $ case repr of
    EnumRepr -> A.TInt
    RecordRepr -> A.TStructPointer [A.TInt]

-- Simple hack to translate builtin function types
-- Otherwise we would have to put builtins in the Program datatype
toANFTypePure :: T.Type -> A.Type
toANFTypePure T.TInt = A.TInt
toANFTypePure (T.TArrow t1 t2) = A.TArrow (toANFTypePure t1) (toANFTypePure t2)
toANFTypePure (T.TAdt adt) = A.TInt

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
  tag <- ctorTagId c
  repr <- ctorRepr c
  e' <- toANFExpr e
  case repr of
    EnumRepr ->
      return (toInteger tag, e')
    RecordRepr -> do
      fields <- ctorFields c
      w <- freshVar "ctor"
      transFields <- mapM toANFType fields
      let ctorType = A.TStructPointer $ A.TInt : transFields
      es <-
        mapM
          (\i -> A.EFetch <$> makeUseVar w ctorType <*> return i)
          [1 .. toInteger $ length fields]
      body <-
        buildExpr
          ( (w, ctorType, A.ECast ctorType v)
              : zip3 xs transFields es
          )
          e'
      return (toInteger tag, body)

toANFExpr :: T.Expr -> ANFTransform A.Expr
toANFExpr T.Annot {T.value = e, T.typ = t} = do
  t' <- toANFType t
  toANFExpr' e t'

toANFExpr' :: T.Expr' -> A.Type -> ANFTransform A.Expr
toANFExpr' e t =
  case e of
    T.EInt n -> return $ A.EValue (A.VInt n)
    T.EVar x -> A.EValue <$> makeUseVar x t
    T.EBinop op e1 e2 ->
      case op of
        T.EagerBinop op ->
          toANFExprName
            e1
            ( \v1 ->
                toANFExprName
                  e2
                  ( \v2 ->
                      return $ A.EEagerBinop op v1 v2
                  )
            )
        T.ShortCircBinop op ->
          A.EShortCircBinop op <$> toANFExpr e1 <*> toANFExpr e2
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
      t <- toANFType $ T.typ e1
      buildExpr [(x, t, e1')] e2'
    T.EFun f x t1 t2 e -> do
      e' <- toANFExpr e
      t1' <- toANFType t1
      t2' <- toANFType t2
      let t' = A.TArrow t1' t2'
      f' <- makeVar f t'
      x' <- makeVar x t1'
      return $ A.EValue $ A.VFun f' x' e'
    T.ECtor c es -> do
      tagId <- ctorTagId c
      repr <- ctorRepr c
      case repr of
        EnumRepr -> return $ A.EValue (A.VInt tagId)
        RecordRepr -> do
          fieldTypes <- (:) <$> return A.TInt <*> mapM (toANFType . T.typ) es
          let rType = A.TStructPointer fieldTypes
          (r, r') <- freshUseVar "r" rType
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
            es <- mapM (toANFClause v) clauses
            let (T.TAdt adt) = T.typ e
            repr <- adtRepr adt
            case repr of
              EnumRepr -> return $ A.ESwitch v es
              RecordRepr -> do
                (t, t') <- freshUseVar "tag" A.TInt
                buildExpr
                  [(t, A.TInt, A.EFetch v 0)]
                  (A.ESwitch t' es)
        )
    T.EPatternMatchingSeq e1 e2 -> A.EPatternMatchingSeq <$> toANFExpr e1 <*> toANFExpr e2
    T.EPatternMatchingError -> return A.EPatternMatchingError
    T.EIf e1 e2 e3 -> A.EIf <$> toANFExpr e1 <*> toANFExpr e2 <*> toANFExpr e3

toANFExprName :: T.Expr -> (A.Value -> ANFTransform A.Expr) -> ANFTransform A.Expr
toANFExprName e k = do
  e1 <- toANFExpr e
  case e1 of
    A.EValue v -> k v
    _ -> do
      t <- toANFType $ T.typ e
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
