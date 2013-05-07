import Control.Applicative ((<$>))
import Data.Maybe (fromMaybe, fromJust)
import Network.CGI

import Planner
import Types
import World

cgiMain :: CGI CGIResult
cgiMain = do
    setHeader "Content-type" "text/plain"
    (holding, world, trees) <- cgiInput
    let plan = findPlan holding world trees
    output (unlines plan)

cgiInput :: CGI (Maybe Block, World, [Tree])
cgiInput = do
    holding <- maybe Nothing getBlock <$> getInput "holding"
    worldStr <- fromMaybe "" <$> getInput "world"
    let world = map (map (fromJust . getBlock) . split ',') . split ';' $ worldStr
    treesStr <- fromMaybe "" <$> getInput "trees"
    let trees = split ';' treesStr
    return (holding, world, trees)

split :: Char -> String -> [String]
split delim str
    | rest == "" = if null token then [] else [token]
    | otherwise  = token : split delim (tail rest)
  where (token, rest) = span (/=delim) str

main :: IO ()
main = runCGI (handleErrors cgiMain)
