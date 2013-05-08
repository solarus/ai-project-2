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

toPDDL :: State -> State -> String
toPDDL initial@(_, iWorld) goal@(_, gWorld) = unlines . execWriter $ do
    line "(define (problem shrdlu)"
    indent $ do
        tellSexp [":domain", "shrdlu"]
        tellSexp (":objects" : blocks ++ floorTiles)
        line "(:init"

        indent $ do
            tellHolding initial

            line ";; All objects are smaller than the floor tiles."
            let smallerThanFloor = [ (o, f) | o <- blocks, f <- floorTiles ]
            mapM_ tellSmaller smallerThanFloor >> ln

            line ";; Some objects are smaller than others."
            mapM_ tellSmaller smallerThan >> ln

            line ";; Some objects are clear."
            forM_ (zipWith (:) floorTiles (map (map name) iWorld)) $ \l ->
                tellSexp ["clear", last l]
            ln

            line ";; Some object are boxes."
            forM_ (map name (filter ((==Box) . form) (concat iWorld))) $ \b ->
                tellSexp ["box", b]
            ln

            line ";; Objects which are _on_ other objects."
            tellOn iWorld >> ln

            line ";; Objects which are _in_ other objects."
            tellIn iWorld >> ln

            line ";; Objects are above floor tiles."
            forM_ (zip floorTiles (map (map name) iWorld)) $ \(f, os) -> do
                tellSexp ["above", f, f]
                mapM_ (\o -> tellSexp ["above", o, f]) os
        line ")"

        -- FIXME: Is this the goal or should we generate some other?
        line "(:goal"
        indent $ do
            line "(and"
            indent $ do
                tellHolding goal
                tellOn gWorld >> ln
                tellIn gWorld
            line ")"
        line ")"
    line ")"
  where
    allBlocks = sortBy (comparing name) (nub (concat iWorld))
    blocks    = map name allBlocks
    floorTiles = map (('f':) . show) [0 .. length iWorld-1 ]

    tellSexp ls = line ("(" ++ intercalate " " ls ++ ")")
    tellSmaller (o1,o2) = tellSexp ["smaller", o1, o2]
    indentStep n = mapWriter (\(a,w) -> (a, map (replicate n ' '++) w))
    indent = indentStep 2
    line = tell . (:[])
    ln   = line ""

    tellHolding = maybe (return ()) (\o -> tellSexp ["holding", name o] >> ln) . fst

    smallerThan = [ (name o1, name o2)
                  | o1 <- allBlocks
                  , o2 <- allBlocks
                  , o1 /= o2
                  , form o1 `notElem` [Pyramid, Ball]
                  , size o1 <= size o2
                  ]

    getOn        = map listToPair . nGrams 2 . reverse
    isOnElems    = concatMap getOn . zipWith (:) floorTiles . map (map name)
    tellOn world = forM_ (isOnElems world) $ \(o1, o2) -> tellSexp ["on", o1, o2]

    getIn _       []  = []
    getIn Nothing (o:rest)
                        -- Boxes are inside themselves
        | form o == Box = (name o, name o) : getIn (Just o) rest
        | otherwise     = getIn Nothing rest
    getIn (Just b) (o:rest)
        | form o == Box = p : getIn (Just o) rest
        | otherwise     = p : getIn (Just b) rest
      where p = (name b, name o)
    -- FIXME: What to do if there are multiple boxes? Currently the
    -- outermost box is the only one that counts.
    isInElems = concatMap (getIn Nothing)
    tellIn world = do
        let elems = isInElems world
            pairToList (a,b) = [a,b]
        forM_ elems $ \(o1, o2) -> tellSexp ["in", o2, o1]
        forM_ (nub (concatMap pairToList elems)) $ \o -> tellSexp ["in-any", o]


-- FIXME: Remove these later
testState :: State
testState = (Nothing, testWorld)

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
