{-# LANGUAGE TupleSections #-}

module Planner where

import Control.Applicative
import Control.Arrow
import Control.Monad.Reader
import Control.Monad.Writer hiding (Any, All)
import Data.Function
import Data.List
import Data.Maybe
import Data.Ord
import System.Exit
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
    if exitCode == ExitSuccess
        then return (lines out ++ map ("# "++ ) (lines problem))
        else return $ ["# Got an error!"] ++ map ("# "++) (lines out ++ lines err ++ lines problem)

findPlan :: State -> [Tree] -> IO [String]
findPlan state@(holding, world) trees = do
    case eGoal of
        Left err   -> return ["# Couldn't find a plan!", "# Error message: " ++ err ]
        Right goal -> do
            plan <- planFromFF (toPDDL (holding, world) goal)
            return $
                [ "# Stupid Haskell planner!"
                , "# Holding: " ++ maybe "Nothing" bName holding
                , "# World: " ++ show (map (map name . snd) world)
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
    eGoal = runReader (findGoal trees) state

findGoal :: [Tree] -> Reader State (Either String Goal)
findGoal []     = return (Left "I can't do that Dave.")
findGoal (t:ts) = do
    res <- tryGoal t
    case res of
        Nothing -> findGoal ts
        Just a  -> return (Right a)

tryGoal :: Tree -> Reader State (Maybe Goal)
tryGoal t = case action of
    "take" -> tryTake (car rest)
    "put"  -> tryPut (car rest)
    "move" -> error "Attila is going to do it"
    _      -> return Nothing
  where
    (Atom action, rest) = uncons (read t)

tryPut :: SExpr -> Reader State (Maybe Goal)
tryPut (List [Atom loc, thingDescr]) = do
    (mh, w) <- ask
    case mh of
        Nothing -> error "Planner.tryPut: Nothing to put!"
        Just h ->
            let bs = findThings w thingDescr
            in return $ Just $ case loc of
                -- FIXME: use Control.Lens instead??????
                "beside"  -> defaultGoal { getBeside  = goalList thingToBlock h bs }
                "leftof"  -> defaultGoal { getLeftOf  = goalList thingToBlock h bs }
                "rightof" -> defaultGoal { getRightOf = goalList thingToBlock h bs }
                "above"   -> defaultGoal { getAbove   = goalList id h bs }
                "ontop"   -> defaultGoal { getOn      = goalList id h bs }
                "under"   -> defaultGoal { getUnder   = goalList thingToBlock h bs }
                "inside"  -> defaultGoal { getIn      = goalList thingToBlock h bs }
  where
    -- Assume any for now
    goalList f h bs = [(The h, Any (map (f . snd) bs))]
tryPut x = error $ "Planner.tryPut: This should not happen!" ++ (show x)

tryMove :: [SExpr] -> Reader World (Maybe Goal)
tryMove = undefined

tryTake :: SExpr -> Reader State (Maybe Goal)
tryTake descr@(List [Atom s, _])
    | s `notElem` ["the", "any"] =
        error "Planner.tryTake: Cannot pick up more than one item!"
    | otherwise = do
        (_,w) <- ask
        let blocks = findThings w descr
        -- The line below crashes if there are floor tiles in `blocks'!
        return (Just (defaultGoal { getHolding = map (thingToBlock . snd) blocks }))
tryTake d = error ("Planner.tryTake: This should not happen!\n" ++ show d)

allThingsAtCol :: World -> Col -> [(Col, Thing)]
allThingsAtCol w c = map (c,) (snd (w !! c))

-- | Given a location expression returns all things that matches the
-- location description.
findLocations :: World -> SExpr -> [(Col, Thing)]
findLocations w (List [Atom loc, thingDescr]) =
    let allBlocks = getBlocks w
        things = findThings w thingDescr
    in case loc of
        "beside"  -> let idxs = nub [i' | (i, _) <- things, i' <- [i-1, i+1], i >= 0, i < length w]
                     in concatMap (allThingsAtCol w) idxs
        "leftof"  -> let idxs = nub [i' | (i, _) <- things, i' <- [0 .. i-1], i < length w]
                     in concatMap (allThingsAtCol w) idxs
        "rightof" -> let idxs = nub [i' | (i, _) <- things, i' <- [i+1 .. (length w) - 1], i >= 0]
                     in concatMap (allThingsAtCol w) idxs
        "above"   -> let colThings = nub [((w!!c), t) | (c, t) <- things ] -- colThings :: [([Thing], Thing)]
                     in concatMap tailAfter colThings
        "ontop"   -> let colThings = nub [((w!!c), t) | (c, t) <- things ] -- colThings :: [([Thing], Thing)]
                     in take 1 $ concatMap tailAfter colThings
        "under"   -> error "TODO findLocations: under"
        "inside"  -> error "TODO findLocations: inside"
        _ -> error "Planner.findLocations: This should not happen!"
findLocations _ s = error $ "Planner.findLocations: Called with " ++ show s

tailAfter ((c, ts), t) = concat $ map (\ts' -> (tag c ts)) (tail $ dropWhile (==t) ts)
tag t ts = map (t, ) ts

findBlocks :: World -> SExpr -> [(Col, Thing)]
findBlocks w (List [Atom "thatis", blockDescr, locDescr]) = intersect blocks locs
  where
    locs   = findLocations w locDescr
    blocks = findBlocks w blockDescr
findBlocks w (List [Atom "block", Atom f, Atom s, Atom c]) =
    map (second TBlock) . formFilter . sizeFilter . colFilter $ allBlocks
  where
    allBlocks = getBlocks w
    genFilter fun str = case reads (capitalize str) of
        (str', ""):_ -> filter ((==str') . fun . snd)
        _            -> id
    formFilter = genFilter form f
    sizeFilter = genFilter size s
    colFilter  = genFilter color c

findThings :: World -> SExpr -> [(Col, Thing)]
findThings w (Atom "floor") = getFloorTiles w
findThings w (List [Atom s, blockDescr]) =
    let blocks = findBlocks w blockDescr
    in case s of
        "the" -> case blocks of
            [x] -> [x]
            _   -> error "Planner.findThings: Found more than one block when parsing 'the' statement."
        _     | s `elem` ["any", "all"] -- && any (isFloorTile . snd) blocks
                -> blocks
              | otherwise
                -> error "Planner.findThings: Found floor tiles when looking for blocks."
findThings _ s = error $ "Planner.findThings: Called with " ++ show s

toPDDL :: State -> Goal -> String
toPDDL initial@(mHolding, iWorld) goal = unlines . execWriter $ do
    line "(define (problem shrdlu)"
    indent $ do
        tellSexp [":domain", "shrdlu"]
        tellSexp (":objects" : blockNames ++ floorTiles)
        line "(:init"

        indent $ do
            tellHolding (maybeToList mHolding)

            line ";; All objects are smaller than the floor tiles."
            let smallerThanFloor = [ (o, f) | o <- blockNames, f <- floorTiles ]
            mapM_ tellSmaller smallerThanFloor >> ln

            line ";; Some objects are smaller than others."
            mapM_ tellSmaller smallerThan >> ln

            line ";; Some objects are clear."
            forM_ (zipWith (:) floorTiles (map (map name) (map snd iWorld))) $ \l ->
                tellSexp ["clear", last l]
            ln

            line ";; Some object are boxes."
            forM_ (map bName (filter ((==Box) . form) (map snd (getBlocks iWorld)))) $ \b ->
                tellSexp ["box", b]
            ln

            line ";; Objects which are _on_ other objects."
            tellOn iWorld >> ln

            line ";; Objects which are _in_ other objects."
            tellIn iWorld >> ln

            line ";; Objects are above and under themselves."
            tellAboveUnder iWorld >> ln

            line ";; The floor tiles are left-of and right-of other floor tiles."
            tellLeftRight iWorld >> ln

            line ";; The floor tiles are also beside directly adjacent floor tiles."
            tellBeside iWorld >> ln

            line ";; Objects are above floor tiles."
            forM_ (zip floorTiles (map (map name . snd) iWorld)) $ \(f, os) -> do
                tellSexp ["stacked-on", f, f]
                mapM_ (\o -> tellSexp ["stacked-on", o, f]) os
        line ")"

        -- FIXME: Is this the goal or should we generate some other?
        line "(:goal"
        indent $ do
            line "(and"
            indent $ do
                tellHolding     (getHolding goal)
                tellGoalIsOn    (getOn goal)
                tellGoalIsIn    (getIn goal)
                tellGoalIsAbove (getAbove goal)
                tellGoalIsUnder (getUnder goal)
                tellGoalLeftOf  (getLeftOf goal)
                tellGoalRightOf (getRightOf goal)
                tellGoalBeside  (getBeside goal)
            line ")"
        line ")"
    line ")"
  where
    allBlocks = sortBy (comparing bName) (maybe [] (:[]) mHolding ++ map snd (getBlocks iWorld))
    blockNames = map bName allBlocks
    floorTiles = map (('f':) . show) [0 .. length iWorld-1]


    tellSexp ls = line ("(" ++ intercalate " " ls ++ ")")
    tellSmaller (o1,o2) = tellSexp ["smaller", o1, o2]
    indentStep n = mapWriter (\(a,w) -> (a, map (replicate n ' '++) w))
    indent = indentStep 2
    line = tell . (:[])
    ln   = line ""

    tellHolding []  = return ()
    tellHolding [o] = tellSexp ["holding", bName o] >> tellSexp ["holding-any"]
    tellHolding os  = tellSexp ("or" : map tellHoldingOne os) >> tellSexp ["holding-any"]
      where
        tellHoldingOne o = unlines (execWriter (tellSexp ["holding", bName o]))

    smallerThan = [ (bName o1, bName o2)
                  | o1 <- allBlocks
                  , o2 <- allBlocks
                  , o1 /= o2
                  , form o2 `notElem` [Pyramid, Ball]
                  , size o1 <= size o2
                  ]

    getOn'       = map listToPair . nGrams 2 . reverse
    isOnElems    = concatMap getOn' . zipWith (:) floorTiles . map (map name . snd)
    tellOn world = forM_ (isOnElems world) $ \(o1, o2) -> tellSexp ["on", o1, o2]

    getIn' _       []  = []
    getIn' m       (TFloorTile _ : rest) = getIn' m rest
    getIn' Nothing (TBlock o:rest)
                        -- Boxes are inside themselves
        | form o == Box = (bName o, bName o) : getIn' (Just o) rest
        | otherwise     = getIn' Nothing rest
    getIn' (Just b) (TBlock o:rest)
        | form o == Box = p : getIn' (Just o) rest
        | otherwise     = p : getIn' (Just b) rest
      where p = (bName b, bName o)
    -- FIXME: What to do if there are multiple boxes? Currently the
    -- outermost box is the only one that counts.
    isInElems = concatMap (getIn' Nothing . snd)
    tellIn world = do
        let elems = isInElems world
            pairToList (a,b) = [a,b]
        forM_ elems $ \(o1, o2) -> tellSexp ["inside", o2, o1]
        forM_ (nub (concatMap pairToList elems)) $ \o -> tellSexp ["inside-any", o]
    tellAboveUnder world = forM_ allBlocks' $ \o -> do
        tellSexp ["above", name o, name o]
        tellSexp ["under", name o, name o]
      where allBlocks' = nubBy ((==) `on` name) (concat (map snd world))

    tellLeftRight world = do
        let floorTiles = sortBy (comparing fst) (getFloorTiles world)
            leftOf     = [ (f1,f2) | (c1,f1) <- floorTiles, (c2,f2) <- floorTiles, c1 < c2 ]
            rightOf    = [ (f1,f2) | (c1,f1) <- floorTiles, (c2,f2) <- floorTiles, c1 > c2 ]
        forM_ leftOf  $ \(f1, f2) -> tellSexp ["left-of",  name f1, name f2 ]
        forM_ rightOf $ \(f1, f2) -> tellSexp ["right-of", name f1, name f2 ]

    tellBeside world = do
        let floorTiles = map snd . sortBy (comparing fst) . getFloorTiles $ world
            neighbors = zip <*> tail $ floorTiles
        forM_ neighbors $ \(f1, f2) -> tellSexp ["beside", name f1, name f2 ]

    tellGoalGen s = mapM_ f
      where
        tellMany s xs w = line ("("++s) >> indent (forM_ xs w) >> line ")"
        tellQuant :: Block -> Quantifier Thing -> Writer [String] ()
        tellQuant x (The y)  = tellSexp [s , bName x, name y]
        tellQuant x (Any ys) = tellMany "or"  ys $ \y -> tellSexp [s, bName x, name y]
        tellQuant x (All ys) = tellMany "and" ys $ \y -> tellSexp [s, bName x, name y]
        f (The x, rest) = tellQuant x rest
        f (Any x, rest) = tellMany "or" (map (,rest) x) $ uncurry tellQuant
        f (All x, rest) = tellMany "or" (map (,rest) x) $ uncurry tellQuant

    -- Here it would be nice with Control.Lens also :(
    tellGoalIsOn    = tellGoalGen "on"
    tellGoalIsIn    = tellGoalGen "inside" . map (second (fmap TBlock))
    tellGoalIsAbove = tellGoalGen "above"
    tellGoalIsUnder = tellGoalGen "under" . map (second (fmap TBlock))
    tellGoalLeftOf  = tellGoalGen "left-of" . map (second (fmap TBlock))
    tellGoalRightOf = tellGoalGen "right-of" . map (second (fmap TBlock))
    tellGoalBeside  = tellGoalGen "beside" . map (second (fmap TBlock))

-- FIXME: perhaps remove these later
------------------------------------

testState :: State
testState = (Nothing, testWorld)

testWorld :: World
testWorld = strToWorld ";a,b;c,d;;e,f,g,h,i;;;j,k;;l,m"

-- toPDDLState :: State -> String
-- toPDDLState initial@(_, iWorld) = unlines . execWriter $ do
--     line "(define (problem shrdlu)"
--     indent $ do
--         tellSexp [":domain", "shrdlu"]
--         tellSexp (":objects" : blocks ++ floorTiles)
--         line "(:init"
--
--         indent $ do
--             tellHolding (maybeToList (fst initial))
--
--             line ";; All objects are smaller than the floor tiles."
--             let smallerThanFloor = [ (o, f) | o <- blocks, f <- floorTiles ]
--             mapM_ tellSmaller smallerThanFloor >> ln
--
--             line ";; Some objects are smaller than others."
--             mapM_ tellSmaller smallerThan >> ln
--
--             line ";; Some objects are clear."
--             forM_ (zipWith (:) floorTiles (map (map name) iWorld)) $ \l ->
--                 tellSexp ["clear", last l]
--             ln
--
--             line ";; Some object are boxes."
--             forM_ (map name (filter ((==Box) . form) (concat iWorld))) $ \b ->
--                 tellSexp ["box", b]
--             ln
--
--             line ";; Objects which are _on_ other objects."
--             tellOn iWorld >> ln
--
--             line ";; Objects which are _in_ other objects."
--             tellIn iWorld >> ln
--
--             line ";; Objects are above themselves."
--             tellAbove iWorld >> ln
--
--             line ";; Objects are above floor tiles."
--             forM_ (zip floorTiles (map (map name) iWorld)) $ \(f, os) -> do
--                 tellSexp ["stacked-on", f, f]
--                 mapM_ (\o -> tellSexp ["stacked-on", o, f]) os
--         line ")"
--     line ")"
--   where
--     allBlocks = sortBy (comparing name) (nub (concat iWorld))
--     blocks    = map name allBlocks
--     floorTiles = map (('f':) . show) [0 .. length iWorld-1 ]
--
--     tellSexp ls = line ("(" ++ intercalate " " ls ++ ")")
--     tellSmaller (o1,o2) = tellSexp ["smaller", o1, o2]
--     indentStep n = mapWriter (\(a,w) -> (a, map (replicate n ' '++) w))
--     indent = indentStep 2
--     line = tell . (:[])
--     ln   = line ""
--
--     tellHolding []  = return ()
--     tellHolding [o] = tellSexp ["holding", name o] >> tellSexp ["holding-any"]
--     tellHolding os  = tellSexp ("or" : map tellHoldingOne os) >> tellSexp ["holding-any"]
--       where
--         tellHoldingOne o = unlines (execWriter (tellSexp ["holding", name o]))
--
--     smallerThan = [ (name o1, name o2)
--                   | o1 <- allBlocks
--                   , o2 <- allBlocks
--                   , o1 /= o2
--                   , form o1 `notElem` [Pyramid, Ball]
--                   , size o1 <= size o2
--                   ]
--
--     getOn        = map listToPair . nGrams 2 . reverse
--     isOnElems    = concatMap getOn . zipWith (:) floorTiles . map (map name)
--     tellOn world = forM_ (isOnElems world) $ \(o1, o2) -> tellSexp ["on", o1, o2]
--
--     getIn _       []  = []
--     getIn Nothing (o:rest)
--                         -- Boxes are inside themselves
--         | form o == Box = (name o, name o) : getIn (Just o) rest
--         | otherwise     = getIn Nothing rest
--     getIn (Just b) (o:rest)
--         | form o == Box = p : getIn (Just o) rest
--         | otherwise     = p : getIn (Just b) rest
--       where p = (name b, name o)
--     -- FIXME: What to do if there are multiple boxes? Currently the
--     -- outermost box is the only one that counts.
--     isInElems = concatMap (getIn Nothing)
--     tellIn world = do
--         let elems = isInElems world
--             pairToList (a,b) = [a,b]
--         forM_ elems $ \(o1, o2) -> tellSexp ["inside", o2, o1]
--         forM_ (nub (concatMap pairToList elems)) $ \o -> tellSexp ["inside-any", o]
--     tellAbove world = forM_ allBlocks $ \o -> tellSexp ["above", name o, name o]
--       where allBlocks' = nubBy ((==) `on` name) (concat world)
