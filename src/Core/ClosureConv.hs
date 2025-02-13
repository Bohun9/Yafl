module Core.ClosureConv where

import qualified ANF.Syntax as A
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Class (lift)
import Control.Monad.Writer
import qualified Core.EscapeAnal as EscapeAnal
import qualified Core.Monad as M
import qualified Core.Syntax as C
import qualified Data.Map as Map

voidPointer :: C.Type
voidPointer = C.TPointer C.TInt

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

convertValue :: A.Value -> (C.Value -> M.ClosureConv C.Expr) -> M.ClosureConv C.Expr
convertValue v k = do
  case v of
    A.VInt n -> k (C.VInt n)
    A.VVar A.VarInfo {A.name = x, A.tag = tag} -> do
      varAccess <- gets M.varAccess
      case Map.lookup tag varAccess of
        Just M.LocalAccess -> k $ C.VLocalVar x
        Just M.NonLocalAccess {M.levelDiff = diff, M.envIndex = M.EnvIndex index} -> do
          envPairs <- sequence $ replicate (diff - 1) $ M.freshVar "loaded_env"
          let (envs, envs') = unzip envPairs
          (v, v') <- M.freshVar "v"
          let es = map (\envVal -> C.EFetch envVal 0) (init $ enclosingEnvValue : envs')
          let e = C.EFetch (last $ enclosingEnvValue : envs') index
          e' <- k v'
          return $
            buildLetExpr
              (zip envs es ++ [(v, e)])
              e'
        Nothing -> error "internal error"
    A.VFun f x e -> do
      (fGlobalName, params, returnType) <- createHoistedFun f x e
      let paramTypes = map (\(C.Param _ t) -> t) params
      (clo, clo') <- M.freshVar "clo"
      e' <- k clo'
      return $
        buildLetExpr
          [(clo, C.EAllocRecord $ convertType $ A.typ f)]
          ( buildSeqExpr
              [ C.EStore clo' 0 (C.VGlobalFun fGlobalName paramTypes returnType),
                C.EStore clo' 1 voidCurEnvValue
              ]
              e'
          )

convertValues :: [A.Value] -> ([C.Value] -> M.ClosureConv C.Expr) -> M.ClosureConv C.Expr
convertValues [] k = k []
convertValues (v : vs) k =
  convertValue
    v
    ( \v' ->
        convertValues vs (\vs' -> k $ v' : vs')
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
    A.EValue v -> convertValue v (\x -> return $ C.EValue x)
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
    A.EBinop op v1 v2 ->
      convertValue
        v1
        ( \v1' ->
            convertValue
              v2
              ( \v2' ->
                  return $ C.EBinop op v1' v2'
              )
        )
    A.EApp v1 v2 ->
      convertValue
        v1
        ( \v1' ->
            convertValue
              v2
              ( \v2' -> do
                  let codeName = "_code"
                      voidCloName = "_void_clo"
                  return $
                    buildLetExpr
                      [ (codeName, C.EFetch v1' 0),
                        (voidCloName, C.ECast voidPointer v1')
                      ]
                      ( C.EApp
                          (C.VLocalVar codeName)
                          [C.VLocalVar voidCloName, v2']
                      )
              )
        )
    A.ESwitch v cs ->
      convertValue
        v
        ( \v' -> do
            let (tags, es) = unzip cs
            es' <- mapM convertExpr es
            return $ C.ESwitch v' (zip tags es')
        )
    A.EPatternMatchingSeq e1 e2 -> C.EPatternMatchingSeq <$> convertExpr e1 <*> convertExpr e2
    A.EPatternMatchingError -> return C.EPatternMatchingError
    A.EMakeRecord ts vs -> do
      (r, r') <- M.freshVar "r"
      convertValues
        vs
        ( \vs' ->
            return $
              buildLetExpr
                [(r, C.EAllocRecord $ C.TPointer $ C.TStruct $ map convertType ts)]
                ( buildSeqExpr
                    (zipWith (C.EStore r') [0 ..] vs')
                    (C.EValue r')
                )
        )
    A.EFetch v i ->
      convertValue
        v
        ( \v' ->
            return $ C.EFetch v' i
        )
    A.ECast t v ->
      convertValue
        v
        ( \v' ->
            return $ C.ECast (convertType t) v'
        )

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
    A.ELet x e1 e2 -> collectBinder x >> collectEscapingVars e1 >> collectEscapingVars e2
    A.EBinop _ _ _ -> return ()
    A.EApp _ _ -> return ()
    A.ESwitch _ cs -> mapM_ collectEscapingVars (map snd cs)
    A.EPatternMatchingSeq e1 e2 -> collectEscapingVars e1 >> collectEscapingVars e2
    A.EPatternMatchingError -> return ()
    A.EMakeRecord _ _ -> return ()
    A.EFetch _ _ -> return ()
    A.ECast _ _ -> return ()

envPayloadTypes :: A.VarInfo -> A.VarInfo -> A.Expr -> M.ClosureConv [C.Type]
envPayloadTypes f x e = do
  indexedTypes <- execWriterT $ collectBinder f >> collectBinder x >> collectEscapingVars e
  let types = Map.elems $ Map.fromList indexedTypes
  return $ map convertType types

cloArgName :: C.Var
cloArgName = "_clo_arg"

cloArgValue :: C.Value
cloArgValue = C.VLocalVar cloArgName

enclosingEnvVoidName :: C.Var
enclosingEnvVoidName = "_void_enclosing_env"

enclosingEnvVoidValue :: C.Value
enclosingEnvVoidValue = C.VLocalVar enclosingEnvVoidName

enclosingEnvName :: C.Var
enclosingEnvName = "_enclosing_env"

enclosingEnvValue :: C.Value
enclosingEnvValue = C.VLocalVar enclosingEnvName

curEnvName :: C.Var
curEnvName = "_cur_env"

curEnvValue :: C.Value
curEnvValue = C.VLocalVar curEnvName

voidCurEnvName :: C.Var
voidCurEnvName = "_void_cur_env"

voidCurEnvValue :: C.Value
voidCurEnvValue = C.VLocalVar voidCurEnvName

createHoistedFun :: A.VarInfo -> A.VarInfo -> A.Expr -> M.ClosureConv (C.Var, [C.Param], C.Type)
createHoistedFun
  fi@A.VarInfo {A.name = f, A.tag = fTag, A.typ = fType}
  xi@A.VarInfo {A.name = x, A.tag = xTag, A.typ = xType}
  e = do
    enclosingEnvType <- reader M.curEnvType
    envPayloadTypes <- envPayloadTypes fi xi e
    let curEnvType = C.TPointer $ C.TStruct $ enclosingEnvType : envPayloadTypes
    fEnvUpdate <- varEnvUpdate fi
    xEnvUpdate <- varEnvUpdate xi
    e' <- convertExpr e
    let f' = C.VLocalVar f
        body =
          buildLetExpr
            [ (f, C.ECast (convertType fType) cloArgValue),
              (enclosingEnvVoidName, C.EFetch f' 1),
              (enclosingEnvName, C.ECast enclosingEnvType enclosingEnvVoidValue),
              (curEnvName, C.EAllocRecord curEnvType),
              (voidCurEnvName, C.ECast voidPointer curEnvValue)
            ]
            ( buildSeqExpr
                ( (C.EStore curEnvValue 0 enclosingEnvValue)
                    : fEnvUpdate
                    ++ xEnvUpdate
                )
                e'
            )
        params = [C.Param cloArgName voidPointer, C.Param x $ convertType xType]
        (A.TArrow _ returnType) = fType
        returnType' = convertType returnType
        hoistedFun =
          C.TopLevelFun
            { C.name = f,
              C.params = params,
              C.returnType = returnType',
              C.body = body
            }
    tell [hoistedFun]
    return (f, params, returnType')

convertProgram :: A.Program -> M.ClosureConv ()
convertProgram (A.Program topLevelExpr) =
  let mainFunVar =
        A.VarInfo
          { A.name = "yafl",
            A.tag = A.Tag (-1),
            A.typ = A.TArrow A.TInt A.TInt
          }
      mainArgVar =
        A.VarInfo
          { A.name = "fake_arg",
            A.tag = A.Tag (-1),
            A.typ = A.TInt
          }
   in void $ createHoistedFun mainFunVar mainArgVar topLevelExpr

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
