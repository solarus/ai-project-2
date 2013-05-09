module World
    ( getBlock
    ) where

import           Data.Map (Map)
import qualified Data.Map as M
import           Types

getBlock :: String -> Maybe Block
getBlock = (`M.lookup` blocks)

blocks :: Map String Block
blocks = M.fromList . map (\x -> (bName x, x)) $
    [ Block { bName = "a", form = Rectangle, size = Tall,   color = Blue,   width = 0.50, height = 1.00 }
    , Block { bName = "b", form = Ball,      size = Small,  color = White,  width = 0.50, height = 0.50 }
    , Block { bName = "c", form = Square,    size = Large,  color = Red,    width = 1.00, height = 1.00 }
    , Block { bName = "d", form = Pyramid,   size = Large,  color = Green,  width = 1.00, height = 1.00 }
    , Block { bName = "e", form = Box,       size = Large,  color = White,  width = 1.00, height = 0.75 }
    , Block { bName = "f", form = Rectangle, size = Wide,   color = Black,  width = 1.00, height = 0.50 }
    , Block { bName = "g", form = Rectangle, size = Wide,   color = Blue,   width = 1.00, height = 0.50 }
    , Block { bName = "h", form = Rectangle, size = Wide,   color = Red,    width = 1.00, height = 0.50 }
    , Block { bName = "i", form = Pyramid,   size = Medium, color = Yellow, width = 0.75, height = 0.75 }
    , Block { bName = "j", form = Box,       size = Large,  color = Red,    width = 1.00, height = 0.75 }
    , Block { bName = "k", form = Ball,      size = Small,  color = Yellow, width = 0.50, height = 0.50 }
    , Block { bName = "l", form = Box,       size = Medium, color = Red,    width = 0.75, height = 0.50 }
    , Block { bName = "m", form = Ball,      size = Medium, color = Blue,   width = 0.75, height = 0.75 }
    ]
