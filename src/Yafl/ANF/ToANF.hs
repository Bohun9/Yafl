module Yafl.ANF.ToANF where

import Control.Monad.Reader
import Control.Monad.State
import qualified Data.Map as Map
import qualified Yafl.ANF.Monad as M
import qualified Yafl.ANF.Syntax as A
import qualified Yafl.Typed.Syntax as T

toANFType :: T.Type -> M.ANFTransform A.Type
toANFType T.TInt = return A.TInt
toANFType T.TBool = return A.TBool
toANFType (T.TArrow t1 t2) = A.TArrow <$> toANFType t1 <*> toANFType t2
toANFType (T.TAdt adt) = do
  repr <- M.adtRepr adt
  return $ case repr of
    M.EnumRepr -> A.TInt
    M.RecordRepr -> A.TStructPointer [A.TInt]

-- Simple hack to translate builtin function types
-- Otherwise we would have to put builtins in the Program datatype
toANFTypePure :: T.Type -> A.Type
toANFTypePure T.TInt = A.TInt
toANFTypePure (T.TArrow t1 t2) = A.TArrow (toANFTypePure t1) (toANFTypePure t2)
toANFTypePure (T.TAdt adt) = A.TInt

buildExpr :: [(A.Var, A.Type, A.Expr)] -> A.Expr -> M.ANFTransform A.Expr
buildExpr defs e =
  foldM
    ( \acc (x, t, e) -> do
        v <- M.makeVar x t
        return $ A.ELet v e acc
    )
    e
    (reverse defs)

toANFClause :: A.Value -> T.Clause -> M.ANFTransform (Integer, A.Expr)
toANFClause v (T.Clause (T.PCtor c xs) e) = do
  tag <- M.ctorTagId c
  repr <- M.ctorRepr c
  e' <- toANFExpr e
  case repr of
    M.EnumRepr ->
      return (toInteger tag, e')
    M.RecordRepr -> do
      fields <- M.ctorFields c
      w <- M.freshVar "ctor"
      transFields <- mapM toANFType fields
      let ctorType = A.TStructPointer $ A.TInt : transFields
      es <-
        mapM
          (\i -> A.EFetch <$> M.makeUseVar w ctorType <*> return i)
          [1 .. toInteger $ length fields]
      body <-
        buildExpr
          ( (w, ctorType, A.ECast ctorType v)
              : zip3 xs transFields es
          )
          e'
      return (toInteger tag, body)

toANFExpr :: T.Expr -> M.ANFTransform A.Expr
toANFExpr T.Annot {T.value = e, T.typ = t} = do
  t' <- toANFType t
  toANFExpr' e t'

toANFExpr' :: T.Expr' -> A.Type -> M.ANFTransform A.Expr
toANFExpr' e t =
  case e of
    T.EInt n -> return $ A.EValue (A.VInt n)
    T.EVar x -> A.EValue <$> M.makeUseVar x t
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
      f' <- M.makeVar f t'
      x' <- M.makeVar x t1'
      return $ A.EValue $ A.VFun f' x' e'
    T.ECtor c es -> do
      tagId <- M.ctorTagId c
      repr <- M.ctorRepr c
      case repr of
        M.EnumRepr -> return $ A.EValue (A.VInt tagId)
        M.RecordRepr -> do
          fieldTypes <- (:) <$> return A.TInt <*> mapM (toANFType . T.typ) es
          let rType = A.TStructPointer fieldTypes
          (r, r') <- M.freshUseVar "r" rType
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
            repr <- M.adtRepr adt
            case repr of
              M.EnumRepr -> return $ A.ESwitch v es
              M.RecordRepr -> do
                (t, t') <- M.freshUseVar "tag" A.TInt
                buildExpr
                  [(t, A.TInt, A.EFetch v 0)]
                  (A.ESwitch t' es)
        )
    T.EMatchSeq e1 e2 -> A.EMatchSeq <$> toANFExpr e1 <*> toANFExpr e2
    T.EMatchError -> return A.EMatchError
    T.EIf e1 e2 e3 -> A.EIf <$> toANFExpr e1 <*> toANFExpr e2 <*> toANFExpr e3

toANFExprName :: T.Expr -> (A.Value -> M.ANFTransform A.Expr) -> M.ANFTransform A.Expr
toANFExprName e k = do
  e1 <- toANFExpr e
  case e1 of
    A.EValue v -> k v
    _ -> do
      t <- toANFType $ T.typ e
      (a, a') <- M.freshUseVar "a" t
      e2 <- k a'
      buildExpr [(a, t, e1)] e2

toANFExprNames :: [T.Expr] -> ([A.Value] -> M.ANFTransform A.Expr) -> M.ANFTransform A.Expr
toANFExprNames [] k = k []
toANFExprNames (e : es) k = toANFExprName e (\v -> toANFExprNames es (\vs -> k $ v : vs))

toANF :: T.Program -> A.Program
toANF (T.Program typeInfo e) =
  let initState =
        M.ANFState
          { M.freshVarId = 0,
            M.freshTagId = 0
          }
      initEnv =
        M.ANFEnv
          { M.typeInfo = typeInfo
          }
   in A.Program $ runReader (evalStateT (toANFExpr e) initState) initEnv
