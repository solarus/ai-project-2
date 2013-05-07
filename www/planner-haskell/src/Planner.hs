module Planner where

import Types
import World

findPlan :: Maybe Block -> World -> [Tree] -> [String]
findPlan holding world trees =
    [ "# Stupid Haskell planner!"
    , "# Holding: " ++ maybe "Nothing" name holding
    , "# World: " ++ show (map (map name) world)
    ]
    ++

    [ "# Tree " ++ show n ++ ": " ++ t
    | (n, t) <- zip [0..] trees
    ]
    ++

    [ "This is a bad move!"
    , "pick " ++ show stacknr
    , "drop " ++ show stacknr
    ]
  where
    stacknr = 1
