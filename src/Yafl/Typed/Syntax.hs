module Yafl.Typed.Syntax
  ( Type (..),
    CtorInfo (..),
    AdtInfo (..),
    TypeInfo (..),
    Annot (..),
    Expr (..),
    Expr' (..),
    Clause (..),
    Pattern (..),
    Program (..),
    Ast.Var (..),
    Ast.Binop (..),
  )
where

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Yafl.Common.Ast as Ast

data Type
  = TInt
  | TBool
  | TArrow Type Type
  | TAdt Ast.Var
  deriving (Show, Eq)

data CtorInfo = CtorInfo
  { tagId :: Int,
    adtName :: Ast.Var,
    fields :: [Type]
  }
  deriving (Show)

data AdtInfo = AdtInfo
  { isEnum :: Bool
  }
  deriving (Show)

data TypeInfo = TypeInfo
  { ctors :: Map.Map Ast.Var CtorInfo,
    adts :: Map.Map Ast.Var AdtInfo
  }
  deriving (Show)

data Annot a = Annot
  { typ :: Type,
    value :: a
  }
  deriving (Show)

type Expr = Annot Expr'

data Expr'
  = EInt Int
  | EVar Ast.Var
  | EBinop Ast.Binop Expr Expr
  | ELet Ast.Var Expr Expr
  | EApp Expr Expr
  | EFun Ast.Var Ast.Var Type Type Expr Expr
  | ECtor Ast.Var [Expr]
  | ECase Expr [Clause]
  | EPatternMatchingSeq Expr Expr
  | EPatternMatchingError
  | EIf Expr Expr Expr
  deriving (Show)

data Clause
  = Clause Pattern Expr
  deriving (Show)

data Pattern
  = PCtor Ast.Var [Ast.Var]
  deriving (Show)

data Program
  = Program TypeInfo Expr
  deriving (Show)
