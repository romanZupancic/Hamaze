module CmdArgs ( interpretArgs
               , HamazeParse (..)
               ) where

import System.Console.GetOpt ( getOpt
                             , usageInfo
                             , ArgOrder ( Permute )
                             , OptDescr ( Option )
                             , ArgDescr ( ReqArg )
                             )

data HamazeParse = HamazeDim (Maybe (Int, Int))
                 | HamazeSeed (Maybe Int)
    deriving Show
    
hamazeArgs :: [OptDescr HamazeParse]
hamazeArgs = [ Option "d" ["dimensions"]
               (ReqArg (\s -> HamazeDim $ tupleMaybe $ toTuple (Nothing) (map toInt (splitOn 'x' s))
                       ) "Integer x Integer")
               "The dimensions of the maze to generate, in nxn format."
             , Option "s" ["seed"]
               (ReqArg (\s -> HamazeSeed $ toInt s) "Single Integer")
               "The seed to use to generate the maze"
             ]

parseArgs :: [String] -> ([HamazeParse], [String], [String])
parseArgs = getOpt Permute hamazeArgs

usage :: String
usage = usageInfo "Usage: hamaze [OPTION...]" hamazeArgs

interpretArgs :: [String] -> Either String [HamazeParse] 
interpretArgs strs = case parseArgs strs of
                         ([], _, []) -> Left usage
                         (a, _, []) -> validateArgs a
                         (_, _, errs) -> Left (concat errs ++ usage)
    where
        validateArgs :: [HamazeParse] -> Either String [HamazeParse]
        validateArgs [] = Right []
        validateArgs (x:xs) = do
            other <- validateArgs xs
            fmap (:other) (inspectArg x) 
        inspectArg (HamazeSeed Nothing) = Left "The seed must be an integer (e.g. '-s 123')."
        inspectArg (HamazeDim Nothing) = Left "The dimensions must be given in an nxn format (e.g. for a 5 by 3 maze, '-d 5x3')"
        inspectArg correct = Right correct

splitOn :: Eq a => a  -> [a] -> [[a]] 
splitOn chr (x:xs) =
    if x == chr then [] : [xs]
    else case splitOn chr xs of
        (r:rs) -> (x:r) : rs
        _ -> [] 
splitOn _ _ = []

toTuple :: a -> [a] -> (a, a)
toTuple _ (x:y:_) = (x, y)
toTuple def _ = (def, def)

toInt :: String -> Maybe Int
toInt str = case reads str of
    [(target, "")] -> Just target
    _ -> Nothing

tupleMaybe :: (Maybe a, Maybe b) -> Maybe (a, b) 
tupleMaybe (Just a, Just b) = Just (a, b)
tupleMaybe _ = Nothing
