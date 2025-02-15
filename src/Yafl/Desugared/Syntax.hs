module Yafl.Desugared.Syntax
  ( Expr,
    Expr' (..),
    Clause (..),
    Pattern,
    Pattern' (..),
    Program (..),
    Ast.Var,
    Ast.Binop (..),
    Ast.EagerBinop (..),
    Ast.ShortCircBinop (..),
    Ast.TypeDef,
    Ast.TypeDef' (..),
    Ast.Constructor,
    Ast.Constructor' (..),
    Ast.Type,
    Ast.Type' (..),
    Node.Node (..),
    Node.mkNode,
    Node.AlexPosn (..),
  )
where

import qualified Yafl.Common.Ast as Ast
import qualified Yafl.Common.Node as Node

type Expr = Node.Node Expr'

data Expr'
  = EInt Int
  | EVar Ast.Var
  | EBinop Ast.Binop Expr Expr
  | ELet Ast.Var Expr Expr
  | EApp Expr Expr
  | EFun Ast.Var Ast.Var Ast.Type Ast.Type Expr Expr
  | ECtor Ast.Var [Expr]
  | ECase Expr [Clause]
  | EPatternMatchingSeq Expr Expr
  | EPatternMatchingError
  | EIf Expr Expr Expr
  deriving (Show)

data Clause
  = Clause Pattern Expr
  deriving (Show)

type Pattern = Node.Node Pattern'

data Pattern'
  = PCtor Ast.Var [Ast.Var]
  deriving (Show)

data Program
  = Program [Ast.TypeDef] Expr
  deriving (Show)
