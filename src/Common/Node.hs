module Common.Node
  ( Node (..),
    mkNode,
    Lexer.AlexPosn (..),
  )
where

import Prettyprinter
import qualified Surface.Lexer as Lexer

data Node a = Node
  { pos :: Lexer.AlexPosn,
    value :: a
  }
  deriving (Show)

mkNode :: Lexer.AlexPosn -> a -> Node a
mkNode p v = Node {pos = p, value = v}

instance Pretty a => Pretty (Node a) where
  pretty (Node {value = value}) = pretty value
