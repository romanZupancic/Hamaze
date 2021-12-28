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

data HamazeParse = HamazeDim (Int, Int)
                 | HamazeSeed Int
    deriving Show
    
hamazeArgs = [ Option "d" ["dimensions"]
               (ReqArg (\s -> HamazeDim $ toTuple (0) (map toInt (splitOn 'x' s))) "Integer x Integer")
               "The dimensions of the maze to generate, in nxn format."
             , Option "s" ["seed"]
               (ReqArg (\s -> HamazeSeed $ toInt s) "Integer")
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

toInt :: String -> Int
toInt str = case reads str of
    [(target, "")] -> target
    _ -> 1 
