module ANF.Syntax
  ( Type (..),
    Value (..),
    Expr (..),
    Program (..),
    Ast.Var (..),
    Ast.Binop (..),
    Tag (..),
    VarInfo (..),
  )
where

import qualified Common.Ast as Ast

newtype Tag = Tag Int
  deriving (Show, Eq, Ord)

data VarInfo = VarInfo
  { name :: Ast.Var,
    tag :: Tag,
    typ :: Type
  }
  deriving (Show)

data Type
  = TInt
  | TArrow Type Type
  | TStructPointer [Type]
  deriving (Show)

data Value
  = VInt Int
  | VVar VarInfo
  | VFun VarInfo VarInfo Expr
  deriving (Show)

data Expr
  = EValue Value
  | ELet VarInfo Expr Expr
  | EBinop Ast.Binop Value Value
  | EApp Value Value
  | ESwitch Value [Expr]
  | EPatternMatchingSeq Expr Expr
  | EPatternMatchingError
  | EMakeRecord [Type] [Value]
  | EFetch Value Integer
  | ECast Type Value
  deriving (Show)

data Program
  = Program Expr
  deriving (Show)
