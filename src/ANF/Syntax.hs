module ANF.Syntax
  ( Type (..),
    Value (..),
    Expr (..),
    Program (..),
    Ast.Var (..),
    Ast.EagerBinop (..),
    Ast.ShortCircBinop (..),
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
  | EEagerBinop Ast.EagerBinop Value Value
  | EShortCircBinop Ast.ShortCircBinop Expr Expr
  | EApp Value Value
  | ESwitch Value [(Integer, Expr)]
  | EPatternMatchingSeq Expr Expr
  | EPatternMatchingError
  | EMakeRecord [Type] [Value]
  | EFetch Value Integer
  | ECast Type Value
  | EIf Expr Expr Expr
  deriving (Show)

data Program
  = Program Expr
  deriving (Show)
