module ANF.Syntax
  ( Type (..),
    Value (..),
    Expr (..),
    Program (..),
    Ast.Var (..),
    Ast.Binop (..),
  )
where

import qualified Common.Ast as Ast

data Type
  = TInt
  | TArrow Type Type
  | TStructPointer [Type]
  deriving (Show)

data Value
  = VInt Int
  | VVar Ast.Var
  | VFun Ast.Var Ast.Var Type Type Expr
  deriving (Show)

data Expr
  = EValue Value
  | ELet Ast.Var Type Expr Expr
  | EBinop Ast.Binop Value Value
  | EApp Value Value
  | ESwitch Value [Expr]
  | EPatternMatchingSeq Expr Expr
  | EPatternMatchingError
  | EMakeRecord [Value]
  | EFetch Value Int
  | ECast Type Value
  deriving (Show)

data Program
  = Program Expr
  deriving (Show)
