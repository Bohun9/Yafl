module ANF.ToANF where

import qualified ANF.Syntax as A
import Control.Monad.Reader
import Control.Monad.State
import qualified Data.Map as Map
import qualified Typed.Syntax as T

newtype ANFState = ANFState {freshId :: Int}

newtype ANFEnv = ANFEnv {typeInfo :: T.TypeInfo}

type ANFTransform = StateT ANFState (Reader ANFEnv)

freshVar :: String -> ANFTransform A.Var
freshVar hint = do
  id <- gets freshId
  modify (\s -> s {freshId = id + 1})
  return $ "_" ++ hint ++ show id

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

buildExpr :: [(A.Var, A.Type, A.Expr)] -> A.Expr -> A.Expr
buildExpr defs e =
  foldr (\(x, t, e) acc -> A.ELet x t e acc) e defs

toANFClause :: A.Value -> T.Clause -> ANFTransform A.Expr
toANFClause v (T.Clause (T.PCtor c xs) e) = do
  fields <- ctorFields c
  w <- freshVar "ctor"
  let transFields = map toANFType fields
  let ctorType = A.TStructPointer $ A.TInt : transFields
  e' <- toANFExpr e
  return $
    buildExpr
      ( (w, ctorType, A.ECast ctorType v)
          : zip3 xs transFields (map (A.EFetch (A.VVar w)) [1 ..])
      )
      e'

toANFExpr :: T.Expr -> ANFTransform A.Expr
toANFExpr T.Annot {T.value = e} = toANFExpr' e

toANFExpr' :: T.Expr' -> ANFTransform A.Expr
toANFExpr' e =
  case e of
    T.EInt n -> return $ A.EValue (A.VInt n)
    T.EVar x -> return $ A.EValue (A.VVar x)
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
      return $ A.ELet x t e1' e2'
    T.EFun f x t1 t2 e1 e2 -> do
      e1' <- toANFExpr e1
      e2' <- toANFExpr e2
      let t1' = toANFType t1
          t2' = toANFType t2
      return $ A.ELet f (A.TArrow t1' t2') (A.EValue (A.VFun f x t1' t2' e1')) e2'
    T.ECtor c es -> do
      r <- freshVar "rec"
      tagId <- ctorTagId c
      let recType = A.TStructPointer (A.TInt : map (toANFType . T.typ) es)
      toANFExprNames
        es
        ( \vs ->
            return $
              buildExpr
                [(r, recType, A.EMakeRecord $ A.VInt tagId : vs)]
                (A.ECast (A.TStructPointer [A.TInt]) (A.VVar r))
        )
    T.ECase e clauses ->
      toANFExprName
        e
        ( \v -> do
            tagVar <- freshVar "tag"
            es <- mapM (toANFClause v) clauses
            return $
              buildExpr
                [(tagVar, A.TInt, A.EFetch v 0)]
                (A.ESwitch (A.VVar tagVar) es)
        )
    T.EPatternMatchingSeq e1 e2 -> A.EPatternMatchingSeq <$> toANFExpr e1 <*> toANFExpr e2
    T.EPatternMatchingError -> return A.EPatternMatchingError

toANFExprName :: T.Expr -> (A.Value -> ANFTransform A.Expr) -> ANFTransform A.Expr
toANFExprName e k = do
  e1 <- toANFExpr e
  case e1 of
    A.EValue v -> k v
    _ -> do
      a <- freshVar "a"
      let t = toANFType $ T.typ e
      e2 <- k $ A.VVar a
      return $ A.ELet a t e1 e2

toANFExprNames :: [T.Expr] -> ([A.Value] -> ANFTransform A.Expr) -> ANFTransform A.Expr
toANFExprNames [] k = k []
toANFExprNames (e : es) k = toANFExprName e (\v -> toANFExprNames es (\vs -> k $ v : vs))

toANF :: T.Program -> A.Program
toANF (T.Program typeInfo e) =
  let initState = ANFState {freshId = 0}
      initEnv = ANFEnv {typeInfo = typeInfo}
   in A.Program $ runReader (evalStateT (toANFExpr e) initState) initEnv
