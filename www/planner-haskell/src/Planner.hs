module Planner where

import Control.Monad.Writer
import Data.List
import Data.Ord

import Types
import Utils
import World

findPlan :: Maybe Block -> World -> [Tree] -> [String]
findPlan holding world trees =
    [ "# Stupid Haskell planner!"
    , "# Holding: " ++ maybe "Nothing" name holding
    , "# World: " ++ show (map (map name) world)
    ]
    ++

    [ "# Tree " ++ show n ++ ": " ++ t
    | (n, t) <- zip [(0::Int)..] trees
    ]
    ++

    [ "This is a bad move!"
    , "pick " ++ show stacknr
    , "drop " ++ show stacknr
    ]
  where
    stacknr = 1 :: Int

toPDDL :: World -> World -> String
toPDDL initial goal = unlines . execWriter $ do
    line "(define (problem shrdlu)"
    indent $ do
        line "(:domain shdrlu)"
        let floorTiles = map (('_':) . show) [1..length initial]
        tellSexp (":objects" : blocks ++ floorTiles)
        line "(:init"

        indent $ do
            line ";; All objects are smaller than the floor tiles."
            let smallerThanFloor = [ (f, o) | o <- blocks, f <- floorTiles ]
            mapM_ tellSmaller smallerThanFloor >> ln

            line ";; Some objects are smaller than others."
            mapM_ tellSmaller smallerThan >> ln

            line ";; Some objects are clear."
            forM_ (zipWith (:) floorTiles (map (map name) initial)) $ \l ->
                tellSexp ["clear", last l]
            ln

            line ";; Objects which are _on_ other objects."
            tellOn initial >> ln

            line ";; Objects which are _in_ other objects."
            tellIn initial
        line ")"

        -- FIXME: Is this the goal or should we generate some other?
        line "(:goal"
        indent $ do
            line "(and"
            indent $ do
                tellOn goal >> ln
                tellIn goal
            line ")"
        line ")"
    line ")"
  where
    allBlocks = sortBy (comparing name) (nub (concat initial))
    blocks    = map name allBlocks

    tellSexp ls = line ("(" ++ intercalate " " ls ++ ")")
    tellSmaller (o1,o2) = tellSexp ["smaller", o1, o2]
    indentStep n = mapWriter (\(a,w) -> (a, map (replicate n ' '++) w))
    indent = indentStep 2
    line = tell . (:[])
    ln   = line ""

    smallerThan = [ (name o1, name o2)
                  | o1 <- allBlocks
                  , o2 <- allBlocks
                  , o1 /= o2
                  , form o1 `notElem` [Pyramid, Ball]
                  , size o1 > size o2
                  ]

    getOn        = map listToPair . map reverse . nGrams 2 . map name . reverse
    isOnElems    = concatMap getOn
    tellOn world = forM_ (isOnElems world) $ \(o1, o2) -> tellSexp ["on", o1, o2]

    getIn _       []  = []
    getIn Nothing (o:rest)
        | form o == Box = getIn (Just o) rest
        | otherwise     = getIn Nothing rest
    getIn (Just b) (o:rest)
        | form o == Box = p : getIn (Just o) rest
        | otherwise     = p : getIn (Just b) rest
      where p = (name b, name o)
    -- FIXME: What to do if there are multiple boxes? Currently the
    -- outermost box is the only one that counts.
    isInElems = concatMap (getIn Nothing)
    tellIn world = forM_ (isInElems world) $ \(o1, o2) -> tellSexp ["in", o1, o2]


-- FIXME: Remove this later
testWorld :: World
testWorld = map (map (fromJust . getBlock) . split ',') . split ';' $ worldStr
  where
    fromJust Nothing  = error "Planner.testWorld: fromJust Nothing"
    fromJust (Just a) = a
    worldStr = ";a,b;c,d;;e,f,g,h,i;;;j,k;;l,m"

    split :: Char -> String -> [String]
    split delim str
        | rest == "" = if null token then [] else [token]
        | otherwise  = token : split delim (tail rest)
      where (token, rest) = span (/=delim) str
