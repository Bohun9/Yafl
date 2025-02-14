module Common.Ast where

import qualified Common.Node as Node
import Prettyprinter

type Var = String

data Binop
  = Add
  | Sub
  | Mul
  | Div
  | Lt
  | Le
  | Gt
  | Ge
  | Eq
  deriving (Show, Eq)

type TypeDef = Node.Node TypeDef'

data TypeDef'
  = TypeDef Var [Constructor]
  deriving (Show)

type Constructor = Node.Node Constructor'

data Constructor'
  = Constructor Var [Type]
  deriving (Show)

type Type = Node.Node Type'

data Type'
  = TInt
  | TBool
  | TArrow Type Type
  | TADT Var
  deriving (Show)

instance Pretty Binop where
  pretty Add = pretty "Add"
  pretty Sub = pretty "Sub"
  pretty Mul = pretty "Mul"
  pretty Div = pretty "Div"
  pretty Lt = pretty "Lt"
  pretty Le = pretty "Le"
  pretty Gt = pretty "Gt"
  pretty Ge = pretty "Ge"
  pretty Eq = pretty "Eq"
