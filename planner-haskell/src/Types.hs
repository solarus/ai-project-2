{-# LANGUAGE DeriveFunctor #-}

module Types
    ( Size(..)
    , Color(..)
    , Form(..)
    , Width
    , Height
    , Thing(..)
    , Block(..)
    , Col
    , World
    , Tree
    , State
    , Quantifier(..)
    , Goal(..)
    , defaultGoal
    , isFloorTile
    , getBlocks
    , getFloorTiles
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

type Col = Int

type World = [(Col, [Thing])]

-- FIXME: Maybe use the Tree type in PGF instead?
type Tree = String

type State = (Maybe Block, World)

data Quantifier a = The a | Any [a] | All [a]
  deriving (Eq, Show, Functor)

data Goal = G
    { getOn          :: [(Quantifier Block, Quantifier Thing)]
    , getIn          :: [(Quantifier Block, Quantifier Block)]
    , getAbove       :: [(Quantifier Block, Quantifier Thing)]
    , getUnder       :: [(Quantifier Block, Quantifier Block)]
    , getLeftOf      :: [(Quantifier Block, Quantifier Block)]
    , getRightOf     :: [(Quantifier Block, Quantifier Block)]
    , getBeside      :: [(Quantifier Block, Quantifier Block)]
    , getHolding     :: [Block]
    , getStackedSame :: [(Col, Block)]
    }
  deriving (Eq, Show)

isFloorTile (TFloorTile _) = True
isFloorTile _              = False

defaultGoal :: Goal
defaultGoal = G [] [] [] [] [] [] [] [] []

getThings :: World -> [(Int, Thing)]
getThings w = [ (c,b) | (c,bs) <- w, b <- bs ]

getBlocks :: World -> [(Int, Block)]
getBlocks w = [ (c,b) | (c,bs) <- w, TBlock b <- bs ]

getFloorTiles :: World -> [(Col, Thing)]
getFloorTiles w = [ (c,f) | (c, (f:_)) <- w ]

thingToBlock :: Thing -> Block
thingToBlock (TBlock b) = b
thingToBlock _          = error "thingToBlock: TODO maybe have a maybe type here instead (pun not intended)"
