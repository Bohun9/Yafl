module Common.Ast where

import qualified Common.Node as Node

type Var = String

data Binop
  = Add
  deriving (Show)

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
  | TArrow Type Type
  | TADT Var
  deriving (Show)
