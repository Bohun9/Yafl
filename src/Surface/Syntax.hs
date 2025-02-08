module Surface.Syntax
  ( Expr, Expr'(..),
    Clause(..),
    Pattern, Pattern'(..),
    Program(..),
    Ast.Var,
    Ast.Binop(..),
    Ast.TypeDef, Ast.TypeDef'(..),
    Ast.Constructor, Ast.Constructor'(..),
    Ast.Type, Ast.Type'(..),
    Node.Node(..),
    Node.AlexPosn(..),
    renameVar
  ) where

import qualified Common.Ast  as Ast
import qualified Common.Node as Node

type Expr = Node.Node Expr'
data Expr'
  = EInt Int
  | EVar Ast.Var
  | EBinop Ast.Binop Expr Expr
  | ELet Ast.Var Expr Expr
  | EApp Expr Expr
  | EFun Ast.Var Ast.Var Expr Expr
  | ECtor Ast.Var [Expr]
  | EMatch Expr [Clause]
  deriving (Show)

data Clause
  = Clause Pattern Expr
  deriving (Show)

type Pattern = Node.Node Pattern'
data Pattern'
  = PVar Ast.Var
  | PCtor Ast.Var [Pattern]
  deriving (Show)

data Program
  = Program [Ast.TypeDef] Expr
  deriving (Show)

renameVar :: Expr -> Ast.Var -> Ast.Var -> Expr
renameVar (Node.Node { Node.pos = p, Node.value = e }) y s =
  Node.mkNode p $ case e of
    EInt n          -> EInt n
    EVar x          -> EVar $ if x == y then s else x
    EBinop op e1 e2 -> EBinop op (renameVar e1 y s) (renameVar e2 y s)
    ELet x e1 e2    ->
      if x == y then
        ELet x (renameVar e1 y s) e2
      else
        ELet x (renameVar e1 y s) (renameVar e2 y s)
    EApp e1 e2      -> EApp (renameVar e1 y s) (renameVar e2 y s)
    EFun f x e1 e2 ->
      let e1' = if f == y || x == y then e1 else renameVar e1 y s
          e2' = if f == y then e2 else renameVar e2 y s
      in
      EFun f x e1' e2'
    ECtor c es      -> ECtor c (map (\e -> renameVar e y s) es)
    EMatch e cs     -> EMatch (renameVar e y s) (map (\c -> renameVarClause c y s) cs)

renameVarClause :: Clause -> Ast.Var -> Ast.Var -> Clause
renameVarClause (Clause pat e) y s =
  Clause pat $ if elem y (freeVarsPattern pat) then e else renameVar e y s

freeVarsPattern :: Pattern -> [Ast.Var]
freeVarsPattern Node.Node { Node.value = p } =
  case p of
    PVar x     -> [x]
    PCtor _ ps -> concat (map freeVarsPattern ps)
