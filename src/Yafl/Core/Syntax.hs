module Yafl.Core.Syntax
  ( Type (..),
    Value (..),
    Expr (..),
    Param (..),
    TopLevelFun (..),
    Program (..),
    Ast.Var (..),
    Ast.EagerBinop (..),
    Ast.ShortCircBinop (..),
  )
where

import qualified Yafl.Common.Ast as Ast

data Type
  = TInt
  | TBool
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
  | EEagerBinop Ast.EagerBinop Value Value
  | EShortCircBinop Ast.ShortCircBinop Expr Expr
  | EApp Value [Value]
  | ESwitch Value [(Integer, Expr)]
  | EMatchSeq Expr Expr
  | EMatchError
  | EAllocRecord Type
  | ESeq Expr Expr
  | EStore Value Integer Value
  | EFetch Value Integer
  | ECast Type Value
  | EIf Expr Expr Expr
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
  = Program [TopLevelFun]
  deriving (Show)
