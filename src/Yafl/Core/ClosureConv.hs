module Yafl.Core.ClosureConv where

import Control.Monad.Cont
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Class (lift)
import Control.Monad.Writer
import qualified Data.Map as Map
import qualified Yafl.ANF.Syntax as A
import qualified Yafl.Core.EscapeAnal as EscapeAnal
import qualified Yafl.Core.Monad as M
import qualified Yafl.Core.Syntax as C

voidPointer :: C.Type
voidPointer = C.TPointer C.TVoid

closureType :: C.Type -> C.Type -> C.Type
closureType t1 t2 =
  C.TPointer
    ( C.TStruct
        [ C.TPointer $ C.TArrow [voidPointer, t1] t2,
          voidPointer
        ]
    )

convertType :: A.Type -> C.Type
convertType t =
  case t of
    A.TInt -> C.TInt
    A.TArrow t1 t2 -> closureType (convertType t1) (convertType t2)
    A.TStructPointer ts -> C.TPointer (C.TStruct $ map convertType ts)

buildLetExpr :: [(C.Var, C.Expr)] -> C.Expr -> C.Expr
buildLetExpr defs e =
  foldr (\(x, e) acc -> C.ELet x e acc) e defs

buildSeqExpr :: [C.Expr] -> C.Expr -> C.Expr
buildSeqExpr es e =
  foldr C.ESeq e es

convertValue :: A.Value -> ContT C.Expr M.ClosureConv C.Value
convertValue v = do
  case v of
    A.VInt n -> return $ C.VInt n
    A.VVar A.VarInfo {A.name = x, A.tag = tag} -> do
      varAccess <- gets M.varAccess
      case Map.lookup tag varAccess of
        Just M.LocalAccess -> return $ C.VLocalVar x
        Just M.EnvAccess {M.levelDiff = diff, M.envIndex = M.EnvIndex index} -> ContT $ \k -> do
          envPairs <- sequence $ replicate (diff - 1) $ M.freshVar "loaded_env"
          let (envs, envs') = unzip envPairs
          (v, v') <- M.freshVar "v"
          let es = map (\envVal -> C.EFetch envVal 0) (init $ outerEnvValue : envs')
          let e = C.EFetch (last $ outerEnvValue : envs') index
          e' <- k v'
          return $
            buildLetExpr
              (zip envs es ++ [(v, e)])
              e'
        Just (M.BuiltinFunAccess t) -> ContT $ \k -> do
          let (A.TArrow t1 t2) = t
          let (C.TPointer recTy) = convertType t
          (clo, clo') <- M.freshVar "clo"
          e' <- k clo'
          return $
            buildLetExpr
              [(clo, C.EAllocRecord $ recTy)]
              ( buildSeqExpr
                  [ C.EStore clo' 0 (C.VGlobalFun x [voidPointer, convertType t1] (convertType t2))
                  ]
                  e'
              )
        Nothing -> error "internal error"
    A.VFun f x e -> ContT $ \k -> do
      (fGlobalName, params, returnType) <- createHoistedFun f x e
      let paramTypes = map (\(C.Param _ t) -> t) params
      let (C.TPointer recTy) = convertType $ A.typ f
      (clo, clo') <- M.freshVar "clo"
      e' <- k clo'
      return $
        buildLetExpr
          [(clo, C.EAllocRecord $ recTy)]
          ( buildSeqExpr
              [ C.EStore clo' 0 (C.VGlobalFun fGlobalName paramTypes returnType),
                C.EStore clo' 1 rawCurEnvValue
              ]
              e'
          )

convertValues :: [A.Value] -> ContT C.Expr M.ClosureConv [C.Value]
convertValues [] = return []
convertValues (v : vs) = ContT $ \k ->
  runContT
    (convertValue v)
    ( \v' ->
        runContT
          (convertValues vs)
          (\vs' -> k $ v' : vs')
    )

varEnvUpdate :: A.VarInfo -> M.ClosureConv [C.Expr]
varEnvUpdate A.VarInfo {A.name = x, A.tag = tag} = do
  varEnvIndex <- gets M.varEnvIndex
  case Map.lookup tag varEnvIndex of
    Just (M.EnvIndex index) ->
      return $ [C.EStore curEnvValue index (C.VLocalVar x)]
    Nothing ->
      return []

convertExpr :: A.Expr -> M.ClosureConv C.Expr
convertExpr e =
  case e of
    A.EValue v -> runContT (convertValue v) (\x -> return $ C.EValue x)
    A.ELet xi@A.VarInfo {A.name = x, A.tag = tag} e1 e2 -> do
      e1' <- convertExpr e1
      e2' <- convertExpr e2
      envUpdate <- varEnvUpdate xi
      return $
        buildLetExpr
          [(x, e1')]
          ( buildSeqExpr
              envUpdate
              e2'
          )
    A.EEagerBinop op v1 v2 ->
      runContT
        ( do
            v1' <- convertValue v1
            v2' <- convertValue v2
            return $ C.EEagerBinop op v1' v2'
        )
        return
    A.EShortCircBinop op e1 e2 ->
      C.EShortCircBinop op <$> convertExpr e1 <*> convertExpr e2
    A.EApp v1 v2 ->
      runContT
        ( do
            v1' <- convertValue v1
            v2' <- convertValue v2
            let codeName = "_code"
                rawCloName = "_raw_clo"
            return $
              buildLetExpr
                [ (codeName, C.EFetch v1' 0),
                  (rawCloName, C.ECast voidPointer v1')
                ]
                ( C.EApp
                    (C.VLocalVar codeName)
                    [C.VLocalVar rawCloName, v2']
                )
        )
        return
    A.ESwitch v cs ->
      runContT
        ( do
            v' <- convertValue v
            let (tags, es) = unzip cs
            es' <- lift $ mapM convertExpr es
            return $ C.ESwitch v' (zip tags es')
        )
        return
    A.EMatchSeq e1 e2 -> C.EMatchSeq <$> convertExpr e1 <*> convertExpr e2
    A.EMatchError -> return C.EMatchError
    A.EMakeRecord ts vs -> do
      (r, r') <- M.freshVar "r"
      runContT
        ( do
            vs' <- convertValues vs
            return $
              buildLetExpr
                [(r, C.EAllocRecord $ C.TStruct $ map convertType ts)]
                ( buildSeqExpr
                    (zipWith (C.EStore r') [0 ..] vs')
                    (C.EValue r')
                )
        )
        return
    A.EFetch v i ->
      runContT
        ( do
            v' <- convertValue v
            return $ C.EFetch v' i
        )
        return
    A.ECast t v ->
      runContT
        ( do
            v' <- convertValue v
            return $ C.ECast (convertType t) v'
        )
        return
    A.EIf e1 e2 e3 -> C.EIf <$> convertExpr e1 <*> convertExpr e2 <*> convertExpr e3

collectBinder :: A.VarInfo -> WriterT [(M.EnvIndex, A.Type)] M.ClosureConv ()
collectBinder A.VarInfo {A.tag = tag, A.typ = typ} = do
  varEnvIndex <- gets M.varEnvIndex
  case Map.lookup tag varEnvIndex of
    Just index -> tell [(index, typ)]
    Nothing -> return ()

collectEscapingVars :: A.Expr -> WriterT [(M.EnvIndex, A.Type)] M.ClosureConv ()
collectEscapingVars e =
  case e of
    A.EValue _ -> return ()
    A.ELet x e1 e2 -> collectBinder x >> mapM_ collectEscapingVars [e1, e2]
    A.EEagerBinop _ _ _ -> return ()
    A.EShortCircBinop _ e1 e2 -> mapM_ collectEscapingVars [e1, e2]
    A.EApp _ _ -> return ()
    A.ESwitch _ cs -> mapM_ collectEscapingVars (map snd cs)
    A.EMatchSeq e1 e2 -> mapM_ collectEscapingVars [e1, e2]
    A.EMatchError -> return ()
    A.EMakeRecord _ _ -> return ()
    A.EFetch _ _ -> return ()
    A.ECast _ _ -> return ()
    A.EIf e1 e2 e3 -> mapM_ collectEscapingVars [e1, e2, e3]

envPayloadTypes :: A.VarInfo -> A.VarInfo -> A.Expr -> M.ClosureConv [C.Type]
envPayloadTypes f x e = do
  indexedTypes <- execWriterT $ collectBinder f >> collectBinder x >> collectEscapingVars e
  let types = Map.elems $ Map.fromList indexedTypes
  return $ map convertType types

cloArgName :: C.Var
cloArgName = "_clo_arg"

cloArgValue :: C.Value
cloArgValue = C.VLocalVar cloArgName

rawOuterEnvName :: C.Var
rawOuterEnvName = "_raw_outer_env"

rawOuterEnvValue :: C.Value
rawOuterEnvValue = C.VLocalVar rawOuterEnvName

outerEnvName :: C.Var
outerEnvName = "_outer_env"

outerEnvValue :: C.Value
outerEnvValue = C.VLocalVar outerEnvName

curEnvName :: C.Var
curEnvName = "_cur_env"

curEnvValue :: C.Value
curEnvValue = C.VLocalVar curEnvName

rawCurEnvName :: C.Var
rawCurEnvName = "_raw_cur_env"

rawCurEnvValue :: C.Value
rawCurEnvValue = C.VLocalVar rawCurEnvName

topLevelFunName :: C.Var
topLevelFunName = "__yafl_toplevel"

createHoistedFun :: A.VarInfo -> A.VarInfo -> A.Expr -> M.ClosureConv (C.Var, [C.Param], C.Type)
createHoistedFun
  fi@A.VarInfo {A.name = f, A.tag = fTag, A.typ = fType}
  xi@A.VarInfo {A.name = x, A.tag = xTag, A.typ = xType}
  e = do
    outerEnvType <- reader M.curEnvType
    envPayloadTypes <- envPayloadTypes fi xi e
    let curEnvType = C.TStruct $ outerEnvType : envPayloadTypes
        curEnvTypePtr = C.TPointer curEnvType
    fEnvUpdate <- varEnvUpdate fi
    xEnvUpdate <- varEnvUpdate xi
    e' <- local (\r -> r {M.curEnvType = curEnvTypePtr}) $ convertExpr e
    let f' = C.VLocalVar f
        body =
          buildLetExpr
            [ (f, C.ECast (convertType fType) cloArgValue),
              (rawOuterEnvName, C.EFetch f' 1),
              (outerEnvName, C.ECast outerEnvType rawOuterEnvValue),
              (curEnvName, C.EAllocRecord curEnvType),
              (rawCurEnvName, C.ECast voidPointer curEnvValue)
            ]
            ( buildSeqExpr
                ( (C.EStore curEnvValue 0 outerEnvValue)
                    : fEnvUpdate
                    ++ xEnvUpdate
                )
                e'
            )
        params = [C.Param cloArgName voidPointer, C.Param x $ convertType xType]
        (A.TArrow _ returnType) = fType
        returnType' = convertType returnType
    fFresh <- if f == topLevelFunName then return f else fst <$> M.freshVar f
    let hoistedFun =
          C.TopLevelFun
            { C.name = fFresh,
              C.params = params,
              C.returnType = returnType',
              C.body = body
            }
    tell [hoistedFun]
    return (fFresh, params, returnType')

convertProgram :: A.Program -> M.ClosureConv ()
convertProgram (A.Program topLevelExpr) = do
  let mainFunVar =
        A.VarInfo
          { A.name = topLevelFunName,
            A.tag = A.Tag (-1),
            A.typ = A.TArrow A.TInt A.TInt
          }
      mainArgVar =
        A.VarInfo
          { A.name = "fake_arg",
            A.tag = A.Tag (-1),
            A.typ = A.TInt
          }
  void $ createHoistedFun mainFunVar mainArgVar topLevelExpr

closureConv :: A.Program -> C.Program
closureConv program =
  let initEnv =
        M.ClosureConvEnv
          { M.curEnvType = C.TPointer $ C.TStruct []
          }
      initState =
        M.ClosureConvState
          { M.varEnvIndex = Map.empty,
            M.varAccess = Map.empty,
            M.freshVarId = 0,
            M.levelEnvSize = Map.empty
          }
      m = (lift $ lift $ EscapeAnal.escapeAnalysis program) >> convertProgram program
      topLevelFuns = evalState (runReaderT (execWriterT m) initEnv) initState
   in C.Program topLevelFuns
