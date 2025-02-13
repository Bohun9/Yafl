module Core.Syntax
  ( Type (..),
    Value (..),
    Expr (..),
    Param (..),
    TopLevelFun (..),
    Program (..),
    Ast.Var (..),
    Ast.Binop (..),
  )
where

import qualified Common.Ast as Ast

data Type
  = TInt
  | TVoid
  | TArrow [Type] Type
  | TStruct [Type]
  | TPointer Type
  deriving (Show)

data Value
  = VInt Int
  | VLocalVar Ast.Var
  | VGlobalFun Ast.Var [Type] Type
  deriving (Show)

data Expr
  = EValue Value
  | ELet Ast.Var Expr Expr
  | EBinop Ast.Binop Value Value
  | EApp Value [Value]
  | ESwitch Value [(Integer, Expr)]
  | EPatternMatchingSeq Expr Expr
  | EPatternMatchingError
  | EAllocRecord Type
  | ESeq Expr Expr
  | EStore Value Integer Value
  | EFetch Value Integer
  | ECast Type Value
  deriving (Show)

data Param
  = Param Ast.Var Type
  deriving (Show)

data TopLevelFun = TopLevelFun
  { name :: Ast.Var,
    params :: [Param],
    returnType :: Type,
    body :: Expr
  }
  deriving (Show)

data Program
  = Program [TopLevelFun] Ast.Var [Type] Type
  deriving (Show)
