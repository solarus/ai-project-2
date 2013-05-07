module World
    ( Size(..)
    , Color(..)
    , Form(..)
    , Width
    , Height
    , Block(..)
    , getBlock
    ) where

import           Data.Char
import           Data.Map (Map, (!))
import qualified Data.Map as M

data Size = Small | Medium | Large | Wide | Tall
  deriving (Eq, Show)

data Color = Black | White | Blue | Green | Yellow | Red
  deriving (Eq, Show)

data Form = Box | Pyramid | Rectangle | Square | Ball
  deriving (Eq, Show)

type Width  = Double
type Height = Double

data Block = Block
    { form :: Form
    , size :: Size
    , color :: Color
    , width :: Width
    , height :: Height
    }
  deriving (Eq, Show)

getBlock :: String -> Block
getBlock = (blocks !)

blocks :: Map String Block
blocks = M.fromList
    [ ("a", Block { form = Rectangle, size = Tall,   color = Blue,   width = 0.50, height = 1.00 } )
    , ("b", Block { form = Ball,      size = Small,  color = White,  width = 0.50, height = 0.50 } )
    , ("c", Block { form = Square,    size = Large,  color = Red,    width = 1.00, height = 1.00 } )
    , ("d", Block { form = Pyramid,   size = Large,  color = Green,  width = 1.00, height = 1.00 } )
    , ("e", Block { form = Box,       size = Large,  color = White,  width = 1.00, height = 0.75 } )
    , ("f", Block { form = Rectangle, size = Wide,   color = Black,  width = 1.00, height = 0.50 } )
    , ("g", Block { form = Rectangle, size = Wide,   color = Blue,   width = 1.00, height = 0.50 } )
    , ("h", Block { form = Rectangle, size = Wide,   color = Red,    width = 1.00, height = 0.50 } )
    , ("i", Block { form = Pyramid,   size = Medium, color = Yellow, width = 0.75, height = 0.75 } )
    , ("j", Block { form = Box,       size = Large,  color = Red,    width = 1.00, height = 0.75 } )
    , ("k", Block { form = Ball,      size = Small,  color = Yellow, width = 0.50, height = 0.50 } )
    , ("l", Block { form = Box,       size = Medium, color = Red,    width = 0.75, height = 0.50 } )
    , ("m", Block { form = Ball,      size = Medium, color = Blue,   width = 0.75, height = 0.75 } )
    ]
