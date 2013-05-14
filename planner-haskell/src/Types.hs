module Types
    ( Size(..)
    , Color(..)
    , Form(..)
    , Width
    , Height
    , Thing(..)
    , Block(..)
    , World
    , Tree
    , State
    , Goal(..)
    , defaultGoal
    , getBlocks
    , name
    , thingToBlock
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

data Thing = TFloorTile String | TBlock Block
  deriving (Eq, Show)

-- TODO: Ord instance based on size?
data Block = Block
    { bName :: String
    , form :: Form
    , size :: Size
    , color :: Color
    , width :: Width
    , height :: Height
    }
  deriving (Eq, Show)

name :: Thing -> String
name (TFloorTile n) = n
name (TBlock b)     = bName b

type World = [[Thing]]

-- FIXME: Maybe use the Tree type in PGF instead?
type Tree = String

type State = (Maybe Block, World)

data Goal = G
    { isOn       :: [(Block, Thing)]
    , isIn       :: [(Block, Block)]
    , getHolding :: [Block]
    }
  deriving (Eq, Show)

defaultGoal :: Goal
defaultGoal = G [] [] []

getBlocks :: World -> [Block]
getBlocks w = [ b | TBlock b <- concat w ]

thingToBlock :: Thing -> Block
thingToBlock (TBlock b) = b
thingToBlock _          = error "thingToBlock: TODO maybe have a maybe type here instead (pun not intended)"
