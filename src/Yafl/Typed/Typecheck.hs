module Yafl.Typed.Typecheck
  ( typecheck,
  )
where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Yafl.Desugared.Syntax as D
import qualified Yafl.Typed.Builtins as Typed.Builtins
import qualified Yafl.Typed.Syntax as T

data TypecheckState = TypecheckState
  { typeInfo :: T.TypeInfo
  }

data TypecheckEnv = TypecheckEnv
  { varTable :: Map.Map D.Var T.Type
  }

data TypeError
  = TypeError D.AlexPosn String

type Typecheck = StateT TypecheckState (ReaderT TypecheckEnv (Except TypeError))

typeError :: D.AlexPosn -> String -> Typecheck a
typeError p msg = throwError $ TypeError p msg

extendEnv :: D.Var -> T.Type -> Typecheck a -> Typecheck a
extendEnv x t = local (\r -> r {varTable = Map.insert x t (varTable r)})

extendEnvMany :: [(D.Var, T.Type)] -> Typecheck a -> Typecheck a
extendEnvMany xts m = foldr (\(x, t) -> extendEnv x t) m xts

insertAdt :: D.Var -> Bool -> Typecheck ()
insertAdt adt isEnum =
  modify
    ( \s ->
        s
          { typeInfo =
              (typeInfo s)
                { T.adts =
                    Map.insert adt (T.AdtInfo {T.isEnum = isEnum}) $ (T.adts . typeInfo) s
                }
          }
    )

insertCtor :: D.Var -> Int -> D.Var -> [T.Type] -> Typecheck ()
insertCtor ctor tag adt fields = do
  let info = T.CtorInfo {T.tagId = tag, T.adtName = adt, T.fields = fields}
  modify (\s -> s {typeInfo = (typeInfo s) {T.ctors = Map.insert ctor info $ (T.ctors . typeInfo) s}})

typecheckType :: D.Type -> Typecheck T.Type
typecheckType D.Node {D.pos = p, D.value = t} =
  case t of
    D.TInt -> return T.TInt
    D.TBool -> return T.TBool
    D.TArrow t1 t2 -> T.TArrow <$> typecheckType t1 <*> typecheckType t2
    D.TADT n -> do
      adts <- gets (T.adts . typeInfo)
      if Map.member n adts
        then return $ T.TAdt n
        else typeError p $ "Unbound type name " ++ n

typecheckCtor :: D.Var -> (Int, D.Constructor) -> Typecheck ()
typecheckCtor adt (tag, D.Node {D.pos = p, D.value = D.Constructor ctor types}) = do
  types' <- mapM typecheckType types
  insertCtor ctor tag adt types'

typecheckTypeDef :: D.TypeDef -> Typecheck ()
typecheckTypeDef D.Node {D.pos = p, D.value = D.TypeDef adt ctors} = do
  let isEnum = all null (map ((\(D.Constructor _ ts) -> ts) . D.value) ctors)
  insertAdt adt isEnum
  mapM_ (typecheckCtor adt) (zip [0 ..] ctors)

typecheckTypeDefs :: [D.TypeDef] -> Typecheck ()
typecheckTypeDefs tdefs = mapM_ typecheckTypeDef tdefs

typecheckExpr :: D.Expr -> Typecheck T.Expr
typecheckExpr D.Node {D.pos = p, D.value = e} = do
  (e', t) <- typecheckExpr' e p
  return $ T.Annot {T.typ = t, T.value = e'}

typecheckExpr2 :: D.Expr -> Typecheck (T.Expr, T.Type)
typecheckExpr2 e = do
  e' <- typecheckExpr e
  return $ (e', T.typ e')

typecheckExpr' :: D.Expr' -> D.AlexPosn -> Typecheck (T.Expr', T.Type)
typecheckExpr' e p =
  case e of
    D.EInt n -> return $ (T.EInt n, T.TInt)
    D.EVar x -> do
      table <- reader varTable
      case Map.lookup x table of
        Just t -> return $ (T.EVar x, t)
        Nothing -> typeError p $ "Unbound variable " ++ x
    D.EBinop op e1 e2 -> do
      (e1', t1) <- typecheckExpr2 e1
      (e2', t2) <- typecheckExpr2 e2
      t <- case op of
        D.EagerBinop op ->
          case op of
            _ | op `elem` [D.Add, D.Sub, D.Mul, D.Div] ->
              case (t1, t2) of
                (T.TInt, T.TInt) -> return $ T.TInt
                _ -> typeError p "Operands should be of type int"
            _ | op `elem` [D.Lt, D.Le, D.Gt, D.Ge, D.Eq] ->
              case (t1, t2) of
                (T.TInt, T.TInt) -> return $ T.TBool
                _ -> typeError p "Operands should be of type int"
            _ -> error "internal error"
        D.ShortCircBinop _ ->
          case (t1, t2) of
            (T.TBool, T.TBool) -> return $ T.TBool
            _ -> typeError p "Operands should be of type bool"
      return $ (T.EBinop op e1' e2', t)
    D.ELet x e1 e2 -> do
      (e1', t1) <- typecheckExpr2 e1
      (e2', t2) <- extendEnv x t1 $ typecheckExpr2 e2
      return $ (T.ELet x e1' e2', t2)
    D.EApp e1 e2 -> do
      (e1', t1) <- typecheckExpr2 e1
      (e2', t2) <- typecheckExpr2 e2
      t <- case t1 of
        T.TArrow t3 t4 ->
          if t2 == t3
            then return t4
            else typeError p $ "This argument has type " ++ show t2 ++ ", but type " ++ show t3 ++ " is expected"
        _ -> typeError p "Only functions can be applied"
      return $ (T.EApp e1' e2', t)
    D.EFun f x t1 t2 e -> do
      t1' <- typecheckType t1
      t2' <- typecheckType t2
      let ft = T.TArrow t1' t2'
      (e', t') <- extendEnvMany [(f, ft), (x, t1')] $ typecheckExpr2 e
      if t' /= t2'
        then typeError p "Function return type mismatch"
        else return ()
      return $ (T.EFun f x t1' t2' e', T.TArrow t1' t2')
    D.ECtor c es -> do
      ets <- mapM typecheckExpr2 es
      let (es', ts) = unzip ets
      ctorsInfo <- gets (T.ctors . typeInfo)
      case Map.lookup c ctorsInfo of
        Nothing -> typeError p $ "Unbound constructor " ++ c
        Just T.CtorInfo {T.adtName = adt, T.fields = fields} ->
          if length es /= length fields
            then typeError p "Arity constructor mismatch"
            else do
              zipWithM_
                ( \t1 t2 ->
                    if t1 /= t2
                      then typeError p "Constructor argument type mismatch"
                      else return ()
                )
                ts
                fields
              return (T.ECtor c es', T.TAdt adt)
    D.ECase e clauses -> do
      (e', t) <- typecheckExpr2 e
      case t of
        T.TAdt adt -> do
          typedClauses <-
            mapM
              ( \(D.Clause D.Node {D.pos = p', D.value = D.PCtor c xs} e) -> do
                  ctorsInfo <- gets (T.ctors . typeInfo)
                  case Map.lookup c ctorsInfo of
                    Just T.CtorInfo {T.adtName = ctorAdt, T.fields = fields} -> do
                      if ctorAdt /= adt then typeError p' $ "Pattern type mismatch" else return ()
                      if length xs /= length fields then typeError p' $ "Pattern arity mismatch" else return ()
                      (e', t) <- extendEnvMany (zip xs fields) $ typecheckExpr2 e
                      return (T.Clause (T.PCtor c xs) e', t)
                    Nothing -> typeError p' $ "Unbound constructor " ++ c
              )
              clauses
          let (clauses', types) = unzip typedClauses
          mapM_
            ( \t ->
                if t /= head types
                  then typeError p $ "Match clauses have different types"
                  else return ()
            )
            types
          return $ (T.ECase e' clauses', head types)
        _ -> typeError p "Expected algebraic data type in a match expression"
    D.EMatchSeq e1 D.Node {D.pos = p, D.value = D.EMatchError} -> do
      (e1', t1) <- typecheckExpr2 e1
      return
        ( T.EMatchSeq
            e1'
            T.Annot {T.typ = T.TInt, T.value = T.EMatchError},
          t1
        )
    D.EMatchSeq e1 e2 -> do
      (e1', t1) <- typecheckExpr2 e1
      (e2', t2) <- typecheckExpr2 e2
      if t1 /= t2
        then typeError p $ "Match clauses have different types"
        else return ()
      return (T.EMatchSeq e1' e2', t1)
    D.EMatchError -> undefined
    D.EIf e1 e2 e3 -> do
      (e1', t1) <- typecheckExpr2 e1
      (e2', t2) <- typecheckExpr2 e2
      (e3', t3) <- typecheckExpr2 e3
      case t1 of
        T.TBool ->
          if t2 == t3
            then return (T.EIf e1' e2' e3', t2)
            else typeError p $ "If clauses have different types"
        _ -> typeError p $ "If guard expression must be of type int"

insertBuiltinTypes :: Typecheck ()
insertBuiltinTypes =
  mapM_
    ( \(adt, ctors) -> do
        zipWithM_ (\tag (ctor, fields) -> insertCtor ctor tag adt fields) [0 ..] ctors
        insertAdt adt (all null (map snd ctors))
    )
    Typed.Builtins.builtinAdts

typecheckProgram :: D.Program -> Typecheck T.Expr
typecheckProgram (D.Program tdefs e) = do
  insertBuiltinTypes
  typecheckTypeDefs tdefs
  (e', t) <- typecheckExpr2 e
  case t of
    T.TAdt "unit" -> return e'
    _ -> typeError (D.pos e) $ "The final value of the program must have type unit"

typecheck :: D.Program -> T.Program
typecheck prog =
  let initTypeInfo =
        T.TypeInfo
          { T.ctors = Map.empty,
            T.adts = Map.empty
          }
      initState =
        TypecheckState
          { typeInfo = initTypeInfo
          }
      initEnv =
        TypecheckEnv
          { varTable = Map.fromList Typed.Builtins.builtinFuns
          }
   in case runExcept $ runReaderT (runStateT (typecheckProgram prog) initState) initEnv of
        Left (TypeError p msg) ->
          error $ "Type error at " ++ show p ++ ": " ++ msg
        Right (e, s) ->
          T.Program (typeInfo s) e
