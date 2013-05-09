module Types
    ( Size(..)
    , Color(..)
    , Form(..)
    , Width
    , Height
    , Block(..)
    , World
    , Tree
    , State
    , Goal(..)
    , defaultGoal
    ) where

-- TODO: Specialized Ord instance
data Size = Small | Tall | Wide | Medium | Large
  deriving (Eq, Show, Read, Ord)

data Color = Black | White | Blue | Green | Yellow | Red
  deriving (Eq, Show, Read)

data Form = Box | Pyramid | Rectangle | Square | Ball
  deriving (Eq, Show, Read)

type Width  = Double
type Height = Double

-- TODO: Ord instance based on size?
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

-- FIXME: Maybe use the Tree type in PGF instead?
type Tree = String

type State = (Maybe Block, World)

data Goal = G
    { isOn       :: [(Block, Block)]
    , isIn       :: [(Block, Block)]
    , getHolding :: [Block]
    }
  deriving (Eq, Show)

defaultGoal :: Goal
defaultGoal = G [] [] []
