module Desugared.Desugar where

import Control.Monad.Except
import Control.Monad.State
import qualified Desugared.Syntax as D
import qualified Surface.Syntax as S

dummyPos :: S.AlexPosn
dummyPos = S.AlexPn (-1) (-1) (-1)

type PMCompiler = StateT Int (Except String)

freshVar :: String -> PMCompiler S.Var
freshVar hint = do
  id <- state (\s -> (s, s + 1))
  return $ "_" ++ hint ++ show id

data Equation
  = Equation [S.Pattern] S.Expr

data VarEquation
  = VarEquation S.Var [S.Pattern] S.Expr

data CtorEquation
  = CtorEquation S.Var [S.Pattern] [S.Pattern] S.Expr

data EquationGroup
  = VarGroup [VarEquation]
  | CtorGroup [CtorEquation]

groupEquations :: [Equation] -> [EquationGroup]
groupEquations [] = []
groupEquations ((Equation ((S.Node {S.value = S.PVar x}) : ps) e) : eqs) =
  let varEq = VarEquation x ps e
   in case groupEquations eqs of
        VarGroup eqs : gs -> VarGroup (varEq : eqs) : gs
        gs -> VarGroup [varEq] : gs
groupEquations ((Equation ((S.Node {S.value = S.PCtor c ps1}) : ps2) e) : eqs) =
  let ctorEq = CtorEquation c ps1 ps2 e
   in case groupEquations eqs of
        CtorGroup eqs : gs -> CtorGroup (ctorEq : eqs) : gs
        gs -> CtorGroup [ctorEq] : gs

matchVar :: [S.Var] -> [VarEquation] -> PMCompiler D.Expr
matchVar (u : us) eqs =
  match us (map (\(VarEquation x ps e) -> Equation ps (S.renameVar e x u)) eqs)

groupCtors :: [CtorEquation] -> PMCompiler [(S.Var, Int, [Equation])]
groupCtors [] = return []
groupCtors (eq@(CtorEquation c ps1 _ _) : eqs) = do
  let sameKind (CtorEquation c' _ _ _) = c == c'
      g' = eq : filter sameKind eqs
  reducedEqs <-
    mapM
      ( \(CtorEquation _ ps1' ps2' e') ->
          if length ps1 == length ps1'
            then return $ Equation (ps1' ++ ps2') e'
            else throwError $ "A different number of subpatterns were specified for the constructor " ++ c
      )
      g'
  let g = (c, length ps1, reducedEqs)
      restEqs = filter (not . sameKind) eqs
  gs <- groupCtors restEqs
  return $ g : gs

matchCtor :: [S.Var] -> [CtorEquation] -> PMCompiler D.Expr
matchCtor (u : us) eqs = do
  gs <- groupCtors eqs
  clauses <-
    mapM
      ( \(c, lenPats, eqs) -> do
          us' <- sequence $ replicate lenPats $ freshVar "u"
          e' <- match (us' ++ us) eqs
          return $ D.Clause (D.mkNode dummyPos (D.PCtor c us')) e'
      )
      gs
  return $ D.mkNode dummyPos $ D.ECase (D.mkNode dummyPos $ D.EVar u) clauses

matchGroup :: [S.Var] -> EquationGroup -> PMCompiler D.Expr
matchGroup us (VarGroup varEqs) = matchVar us varEqs
matchGroup us (CtorGroup ctorEqs) = matchCtor us ctorEqs

match :: [S.Var] -> [Equation] -> PMCompiler D.Expr
match [] [] = error "internal error"
match [] (Equation _ e : _) = desugarExpr e
match (_ : _) [] = return $ D.mkNode dummyPos D.EPatternMatchingError
match (u : us) eqs = do
  es' <- mapM (matchGroup (u : us)) (groupEquations eqs)
  let (e : es) = reverse es'
  return $ foldl (\acc e -> D.mkNode dummyPos $ D.EPatternMatchingSeq e acc) e es

desugarExpr :: S.Expr -> PMCompiler D.Expr
desugarExpr S.Node {S.pos = p, S.value = e} = do
  e' <- desugarExpr' e p
  return $ D.Node {D.pos = p, D.value = e'}

desugarExpr' :: S.Expr' -> S.AlexPosn -> PMCompiler D.Expr'
desugarExpr' e p =
  case e of
    S.EInt n -> return $ D.EInt n
    S.EVar n -> return $ D.EVar n
    S.EBinop op e1 e2 -> D.EBinop <$> return op <*> desugarExpr e1 <*> desugarExpr e2
    S.ELet x e1 e2 -> D.ELet <$> return x <*> desugarExpr e1 <*> desugarExpr e2
    S.EApp e1 e2 -> D.EApp <$> desugarExpr e1 <*> desugarExpr e2
    S.EFun f x t1 t2 e1 e2 -> D.EFun <$> return f <*> return x <*> return t1 <*> return t2 <*> desugarExpr e1 <*> desugarExpr e2
    S.ECtor c es -> D.ECtor <$> return c <*> mapM desugarExpr es
    S.EMatch e clauses -> do
      e' <- desugarExpr e
      u <- freshVar "match"
      e1 <- match [u] (map (\(S.Clause p e) -> Equation [p] e) clauses)
      let e2 = D.mkNode p $ D.EPatternMatchingSeq e1 (D.mkNode p D.EPatternMatchingError)
          e3 = D.ELet u e' e2
      return e3
    S.EIf e1 e2 e3 -> D.EIf <$> desugarExpr e1 <*> desugarExpr e2 <*> desugarExpr e3

desugarProgram :: S.Program -> PMCompiler D.Program
desugarProgram (S.Program typeDefs e) =
  D.Program <$> return typeDefs <*> desugarExpr e

desugar :: S.Program -> D.Program
desugar prog = case runExcept $ evalStateT (desugarProgram prog) 0 of
  Left msg -> error msg
  Right r -> r
