module Planner where

import Control.Monad.Reader
import Control.Monad.Writer
import Data.Function
import Data.List
import Data.Maybe
import Data.Ord
import System.IO
import System.IO.Temp
import System.Process

import SExpr
import Types
import Utils
import World

planFromFF :: String -> IO [String]
planFromFF problem = withSystemTempFile "shrdlu.problem." $ \fp h -> do
    hPutStr h problem
    hClose h
    let ff = "../bin/ff-wrapper.sh"
        args = [fp]
    (exitCode, out, err) <- readProcessWithExitCode ff args ""
    if null
        then return (lines out)
        else return $ ["# Got an error!"] ++ lines err

findPlan :: Maybe Block -> World -> [Tree] -> IO [String]
findPlan holding world trees = do
    case eGoal of
        Left err   -> return ["# Couldn't find a plan!", "# Error message: " ++ err ]
        Right goal -> do
            plan <- planFromFF (toPDDL (holding, world) goal)
            return $
                [ "# Stupid Haskell planner!"
                , "# Holding: " ++ maybe "Nothing" name holding
                , "# World: " ++ show (map (map name) world)
                ]
                ++

                [ "# Tree " ++ show n ++ ": " ++ show (read t :: SExpr)
                | (n, t) <- zip [(0::Int)..] trees
                ]
                ++

                [ "# Goal:", "# " ++ show goal ]
                ++

                plan
  where
    eGoal = runReader (findGoal trees) world

findGoal :: [Tree] -> Reader World (Either String Goal)
findGoal []     = return (Left "I can't do that Dave.")
findGoal (t:ts) = do
    res <- tryGoal t
    case res of
        Nothing -> findGoal ts
        Just a  -> return (Right a)

tryGoal :: String -> Reader World (Maybe Goal)
tryGoal t = case action of
    "take" -> tryTake rest
    _      -> return Nothing
  where
    t' = read t
    List (Atom action : rest) = t'

tryTake :: [SExpr] -> Reader World (Maybe Goal)
tryTake [List (Atom s : matching : [])]
    | s `notElem` ["the", "any"] =
        error "Planner.tryTake: Cannot pick up more than one item!"
    | otherwise = do
        w <- ask
        case findMatching w matching of
            xs@(_:_) | s == "any" ->
                return (Just (defaultGoal { getHolding = xs }))
            (x:xs) | s == "the" && null xs ->
                return (Just (defaultGoal { getHolding = [x] }))
            _                                              ->
                return Nothing
tryTake _ = error "Planner.tryTake: This should not happen!"

findMatching :: World -> SExpr -> [Block]
findMatching w matching =
    let allBlocks = sortBy (comparing name) (nub (concat w))
    in case matching of
        List (Atom "block" : rest) -> formFilter . sizeFilter . colFilter $ allBlocks
          where
            [Atom f, Atom s, Atom c] = rest
            genFilter fun str = case reads (capitalize str) of
                (str', ""):_ -> filter ((==str') . fun)
                _            -> id
            formFilter = genFilter form f
            sizeFilter = genFilter size s
            colFilter  = genFilter color c
        List [Atom "thatis", b, _p] -> posFilter blocks
          where
            blocks = findMatching w b
            posFilter = id

toPDDL :: State -> Goal -> String
toPDDL initial@(_, iWorld) goal = unlines . execWriter $ do
    line "(define (problem shrdlu)"
    indent $ do
        tellSexp [":domain", "shrdlu"]
        tellSexp (":objects" : blocks ++ floorTiles)
        line "(:init"

        indent $ do
            tellHolding (maybeToList (fst initial))

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

            line ";; Objects are above themselves."
            tellAbove iWorld >> ln

            line ";; Objects are above floor tiles."
            forM_ (zip floorTiles (map (map name) iWorld)) $ \(f, os) -> do
                tellSexp ["stacked-on", f, f]
                mapM_ (\o -> tellSexp ["stacked-on", o, f]) os
        line ")"

        -- FIXME: Is this the goal or should we generate some other?
        line "(:goal"
        indent $ do
            line "(and"
            indent $ do
                tellHolding (getHolding goal)
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

    tellHolding []  = return ()
    tellHolding [o] = tellSexp ["holding", name o] >> tellSexp ["holding-any"]
    tellHolding os  = tellSexp ("or" : map tellHoldingOne os) >> tellSexp ["holding-any"]
      where
        tellHoldingOne o = unlines (execWriter (tellSexp ["holding", name o]))

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
        forM_ elems $ \(o1, o2) -> tellSexp ["inside", o2, o1]
        forM_ (nub (concatMap pairToList elems)) $ \o -> tellSexp ["inside-any", o]
    tellAbove world = forM_ allBlocks $ \o -> tellSexp ["above", name o, name o]
      where allBlocks' = nubBy ((==) `on` name) (concat world)


-- FIXME: Remove these later
testState :: State
testState = (Nothing, testWorld)

testWorld :: World
testWorld = map (map (fromJust . getBlock) . split ',') . split ';' $ worldStr
  where
    worldStr = ";a,b;c,d;;e,f,g,h,i;;;j,k;;l,m"

    split :: Char -> String -> [String]
    split delim str
        | rest == "" = if null token then [] else [token]
        | otherwise  = token : split delim (tail rest)
      where (token, rest) = span (/=delim) str
