module CmdArgs ( usage
               , parseArgs
               , HamazeParse (..)
               ) where

import System.Console.GetOpt ( getOpt
                             , usageInfo
                             , ArgOrder ( Permute )
                             , OptDescr ( Option )
                             , ArgDescr ( ReqArg
                                        , OptArg
                                        )
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
               (OptArg (\s -> HamazeSeed $ case s of
                               Nothing -> Nothing
                               Just str -> toInt str
                       ) "Integer")
               "The seed to use to generate the maze"
             ]

parseArgs :: [String] -> ([HamazeParse], [String], [String])
parseArgs = getOpt Permute hamazeArgs

usage :: String
usage = usageInfo "Usage: hamaze [OPTION...]" hamazeArgs

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
