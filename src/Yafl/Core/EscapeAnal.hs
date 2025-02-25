module Yafl.Core.EscapeAnal where

import Control.Monad.Reader
import Control.Monad.State
import qualified Data.Map as Map
import qualified Yafl.ANF.Builtins as ANF.Builtins
import qualified Yafl.ANF.Syntax as A
import qualified Yafl.ANF.ToANF
import qualified Yafl.Core.Monad as M

varEscapes :: M.Level -> A.Tag -> M.EscapeAnal M.EnvIndex
varEscapes level tag = do
  varEnvIndex <- gets M.varEnvIndex
  case Map.lookup tag varEnvIndex of
    Just index -> return index
    Nothing -> do
      levelEnvSize <- gets M.levelEnvSize
      let envSize = levelEnvSize Map.! level
      let varIndex = M.EnvIndex envSize
      modify
        ( \s ->
            s
              { M.varEnvIndex = Map.insert tag varIndex varEnvIndex,
                M.levelEnvSize = Map.insert level (envSize + 1) levelEnvSize
              }
        )
      return varIndex

incLevel :: M.EscapeAnal a -> M.EscapeAnal a
incLevel m = do
  (M.Level x) <- reader M.curLevel
  let nestedLevel = M.Level $ x + 1
  modify (\s -> s {M.levelEnvSize = Map.insert nestedLevel 1 (M.levelEnvSize s)})
  local (\r -> r {M.curLevel = nestedLevel}) m

analyseValue :: A.Value -> M.EscapeAnal ()
analyseValue v =
  case v of
    A.VInt _ -> return ()
    A.VVar A.VarInfo {A.name = x, A.tag = useTag} -> do
      useLevel <- reader M.curLevel
      entry <- M.lookupVarTable x
      case entry of
        M.UserDefined defLevel defTag ->
          if defLevel < useLevel
            then do
              defVarIndex <- varEscapes defLevel defTag
              let levelDiff (M.Level a) (M.Level b) = a - b
              M.insertVarAccess
                useTag
                M.EnvAccess
                  { M.levelDiff = levelDiff useLevel defLevel,
                    M.envIndex = defVarIndex
                  }
            else M.insertVarAccess useTag M.LocalAccess
        M.BuiltinFun t ->
          M.insertVarAccess useTag $ M.BuiltinFunAccess t
    A.VFun A.VarInfo {A.name = f, A.tag = fTag} A.VarInfo {A.name = x, A.tag = xTag} e ->
      incLevel $ M.extendVarTable f fTag $ M.extendVarTable x xTag $ analyseExpr e

analyseExpr :: A.Expr -> M.EscapeAnal ()
analyseExpr e =
  case e of
    A.EValue v -> analyseValue v
    A.ELet A.VarInfo {A.name = x, A.tag = tag} e1 e2 -> do
      analyseExpr e1
      M.extendVarTable x tag $ analyseExpr e2
    A.EEagerBinop _ v1 v2 -> analyseValue v1 >> analyseValue v2
    A.EShortCircBinop _ e1 e2 -> analyseExpr e1 >> analyseExpr e2
    A.EApp v1 v2 -> analyseValue v1 >> analyseValue v2
    A.ESwitch v cs -> analyseValue v >> mapM_ analyseExpr (map snd cs)
    A.EMatchSeq e1 e2 -> analyseExpr e1 >> analyseExpr e2
    A.EMatchError -> return ()
    A.EMakeRecord _ vs -> mapM_ analyseValue vs
    A.EFetch v _ -> analyseValue v
    A.ECast _ v -> analyseValue v
    A.EIf e1 e2 e3 -> analyseExpr e1 >> analyseExpr e2 >> analyseExpr e3

analyseProgram :: A.Program -> M.EscapeAnal ()
analyseProgram (A.Program e) = incLevel (analyseExpr e)

escapeAnalysis :: A.Program -> State M.ClosureConvState ()
escapeAnalysis program =
  let initEnv =
        M.EscapeAnalEnv
          { M.varTable = Map.fromList $ map (\(n, t) -> (n, M.BuiltinFun t)) ANF.Builtins.builtinFuns,
            M.curLevel = M.Level (-1)
          }
   in runReaderT (analyseProgram program) initEnv
