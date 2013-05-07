module Utils where

nGrams :: Int -> [a] -> [[a]]
nGrams n = takeWhile ((==n) . length) . map (take n) . iterate (drop 1)

listToPair :: [a] -> (a, a)
listToPair [a,b] = (a,b)
listToPair _     = error "Utils.listToPair: Need to be exactly two elements in the list"
