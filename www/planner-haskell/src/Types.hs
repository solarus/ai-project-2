module Types
    ( Size(..)
    , Color(..)
    , Form(..)
    , Width
    , Height
    , Block(..)
    , World
    , Tree
    ) where

data Size = Small | Medium | Large | Wide | Tall
  deriving (Eq, Show)

data Color = Black | White | Blue | Green | Yellow | Red
  deriving (Eq, Show)

data Form = Box | Pyramid | Rectangle | Square | Ball
  deriving (Eq, Show)

type Width  = Double
type Height = Double

data Block = Block
    { name :: String
    , form :: Form
    , size :: Size
    , color :: Color
    , width :: Width
    , height :: Height
    }
  deriving (Eq, Show)

type World = [[Block]]

-- Maybe use the Tree type in PGF instead?
type Tree = String
