module Planner where

import Control.Monad.Writer
import Data.List
import Data.Ord

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
    | (n, t) <- zip [(0::Int)..] trees
    ]
    ++

    [ "This is a bad move!"
    , "pick " ++ show stacknr
    , "drop " ++ show stacknr
    ]
  where
    stacknr = 1

toPDDL :: World -> String
toPDDL ws = execWriter $ do
    tellLn "(define (problem shrdlu)"
    tellLn "  (:domain shdrlu)"

    let allBlocks = sortBy (comparing name) (nub (concat ws))
        blocks    = map name allBlocks
        floor     = map (('f':) . show) [1..length ws]
    tellLn $ "  (:objects " ++ intercalate " " (blocks ++ floor) ++ " )"

    tellLn "  (:init"

    tellLn "    ;; Everything is smaller than the floor tiles."
    let smallerThanFloor = [ (f, o) | o <- blocks, f <- floor ]
    mapM_ tellSmaller smallerThanFloor

    tellLn "\n    ;; Some objects are smaller than others."
    let smallerThan = [ (name o1, name o2)
                      | o1 <- allBlocks
                      , o2 <- allBlocks
                      , o1 /= o2
                      , form o1 `notElem` [Pyramid, Ball]
                      , size o1 > size o2
                      ]
    mapM_ tellSmaller smallerThan

    tellLn "\n    ;; Objects which are on other objects."
    let isOnFun      = map toPair . nGrams 2 . map name . reverse
        nGrams n     = takeWhile ((==n) . length) . map (take n) . iterate (drop 1)
        toPair [a,b] = (a,b)
        isOn         = concatMap isOnFun ws
    forM_ isOn $ \(o1, o2) ->
        tellLn $ "    (on " ++ o1 ++ " " ++ o2 ++ ")"

    tellLn "\n    ;; Objects which are in other objects."
    -- FIXME: What to do if there are multiple boxes? Currently the
    -- outermost box is the one that counts
    let isInFun _       []  = []
        isInFun _       [_] = []
        isInFun Nothing (o:rest)
            | form o == Box = isInFun (Just o) rest
            | otherwise     = isInFun Nothing rest
        isInFun (Just b) (o:rest)
            | form o == Box = p : isInFun (Just o) rest
            | otherwise     = p : isInFun (Just b) rest
          where p = (name b, name o)
        isIn = concatMap (isInFun Nothing) ws
    forM_ isIn $ \(o1, o2) ->
        tellLn $ "    (in " ++ o1 ++ " " ++ o2 ++ ")"
    tellLn "  )"

    tellLn "\n    ;; TODO fix goal"
    tellLn "  (:goal"
    tellLn "    (and"
    tellLn "      (on a b)"
    tellLn "      (on b c))))"
  where
    tellSmaller (o1,o2) = tellLn ("    (smaller " ++ o1 ++ " " ++ o2 ++ ")")
    indent n s = replicate n ' ' ++ s
    tellLn s = tell (s ++ "\n")

-- FIXME: Remove this later
testWorld :: World
testWorld = map (map (fromJust . getBlock) . split ',') . split ';' $ worldStr
  where
    fromJust (Just a) = a
    worldStr = ";a,b;c,d;;e,f,g,h,i;;;j,k;;l,m"

    split :: Char -> String -> [String]
    split delim str
        | rest == "" = if null token then [] else [token]
        | otherwise  = token : split delim (tail rest)
      where (token, rest) = span (/=delim) str
