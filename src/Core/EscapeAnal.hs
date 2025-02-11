module Core.EscapeAnal where

import qualified ANF.Syntax as A
import Control.Monad.Reader
import Control.Monad.State
import qualified Core.Monad as M
import qualified Data.Map as Map

extendVarTable :: A.Var -> A.Tag -> M.EscapeAnal a -> M.EscapeAnal a
extendVarTable x tag m = do
  level <- reader M.curLevel
  local (\r -> r {M.varTable = Map.insert x (level, tag) (M.varTable r)}) m

lookupVarTable :: A.Var -> M.EscapeAnal (M.Level, A.Tag)
lookupVarTable x = do
  varTable <- reader M.varTable
  case Map.lookup x varTable of
    Just r -> return r
    Nothing -> error "internal error"

insertVarAccess :: A.Tag -> M.VarAccess -> M.EscapeAnal ()
insertVarAccess tag access =
  modify (\s -> s {M.varAccess = Map.insert tag access (M.varAccess s)})

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
  modify (\s -> s {M.levelEnvSize = Map.insert nestedLevel 0 (M.levelEnvSize s)})
  local (\r -> r {M.curLevel = nestedLevel}) m

analyseValue :: A.Value -> M.EscapeAnal ()
analyseValue v =
  case v of
    A.VInt _ -> return ()
    A.VVar A.VarInfo {A.name = x, A.tag = useTag} -> do
      useLevel <- reader M.curLevel
      (defLevel, defTag) <- lookupVarTable x
      if defLevel < useLevel
        then do
          defVarIndex <- varEscapes defLevel defTag
          let levelDiff (M.Level a) (M.Level b) = a - b
          insertVarAccess
            useTag
            M.NonLocalAccess
              { M.levelDiff = levelDiff useLevel defLevel,
                M.envIndex = defVarIndex
              }
        else insertVarAccess useTag M.LocalAccess
    A.VFun A.VarInfo {A.name = f, A.tag = fTag} A.VarInfo {A.name = x, A.tag = xTag} e ->
      incLevel $ extendVarTable f fTag $ extendVarTable x xTag $ analyseExpr e

analyseExpr :: A.Expr -> M.EscapeAnal ()
analyseExpr e =
  case e of
    A.EValue v -> analyseValue v
    A.ELet A.VarInfo {A.name = x, A.tag = tag} e1 e2 -> do
      analyseExpr e1
      extendVarTable x tag $ analyseExpr e2
    A.EBinop _ v1 v2 -> analyseValue v1 >> analyseValue v2
    A.EApp v1 v2 -> analyseValue v1 >> analyseValue v2
    A.ESwitch v es -> analyseValue v >> mapM_ analyseExpr es
    A.EPatternMatchingSeq e1 e2 -> analyseExpr e1 >> analyseExpr e2
    A.EPatternMatchingError -> return ()
    A.EMakeRecord _ vs -> mapM_ analyseValue vs
    A.EFetch v _ -> analyseValue v
    A.ECast _ v -> analyseValue v

analyseProgram :: A.Program -> M.EscapeAnal ()
analyseProgram (A.Program e) = incLevel (analyseExpr e)

escapeAnalysis :: A.Program -> State M.ClosureConvState ()
escapeAnalysis program =
  let initEnv =
        M.EscapeAnalEnv
          { M.varTable = Map.empty,
            M.curLevel = M.Level (-1)
          }
   in runReaderT (analyseProgram program) initEnv
