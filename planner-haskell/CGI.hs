import Control.Applicative ((<$>))
import Control.DeepSeq
import Control.Monad.Trans
import Data.Maybe (fromMaybe)
import Network.CGI

import Planner
import Types
import World

cgiMain :: CGI CGIResult
cgiMain = do
    (state, trees) <- cgiInput
    plan <- lift (findPlan state trees)

    plan `deepseq` do
        setHeader "Content-type" "text/plain"
        output (unlines plan)

cgiInput :: CGI (State, [Tree])
cgiInput = do
    holding <- maybe Nothing getBlock <$> getInput "holding"
    worldStr <- fromMaybe "" <$> getInput "world"
    let world = strToWorld worldStr
    treesStr <- fromMaybe "" <$> getInput "trees"
    let trees = split ';' treesStr
    return ((holding, world), trees)

split :: Char -> String -> [String]
split delim str
    | rest == "" = if null token then [] else [token]
    | otherwise  = token : split delim (tail rest)
  where (token, rest) = span (/=delim) str

main :: IO ()
main = runCGI $ do
    res <- tryCGI cgiMain
    case res of
        Left err -> do
            setHeader "Content-type" "text/plain"
            output $ "Got an error!\n" ++ show err
        Right a  -> return a
