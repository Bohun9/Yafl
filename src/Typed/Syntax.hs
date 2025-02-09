module Typed.Syntax where

import qualified Common.Ast as Ast
import qualified Data.Map as Map
import qualified Data.Set as Set

data Type
  = TInt
  | TArrow Type Type
  | TAdt Ast.Var
  deriving (Show, Eq)

data CtorInfo = CtorInfo
  { tagId :: Int,
    adtName :: Ast.Var,
    fields :: [Type]
  }
  deriving (Show)

data TypeInfo = TypeInfo
  { ctors :: Map.Map Ast.Var CtorInfo,
    adts :: Set.Set Ast.Var
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
  | EFun Ast.Var Ast.Var Expr Expr
  | ECtor Ast.Var [Expr]
  | ECase Expr [Clause]
  | EPatternMatchingSeq Expr Expr
  | EPatternMatchingError
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
