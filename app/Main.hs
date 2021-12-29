module Main where

import System.Environment ( getArgs )
import System.Random ( newStdGen
                     , mkStdGen
                     , StdGen ()
                     )
import CmdArgs ( interpretArgs
               , HamazeParse (..)
               )
import MazeGen ( genMazeFromScratch )

import MazeDisplay ( drawSingleWallMaze )


data HamazeGenOpts = HamazeGenOpts { hamazeDimension :: (Int, Int)
                                   , hamazeSeed :: StdGen
                                   }
    deriving Show

getGenOpts :: [HamazeParse] -> IO (HamazeGenOpts)
getGenOpts args = case getGenOpts' args (HamazeGenOpts (0, 0) (mkStdGen 0)) of
                      (True, opts) -> do
                          seed <- newStdGen
                          return $ opts { hamazeSeed = seed }
                      (False, opts) -> return opts
            

getGenOpts' :: [HamazeParse] -> HamazeGenOpts -> (Bool, HamazeGenOpts)
getGenOpts' [] opts = (True, opts)
getGenOpts' (x:xs) opts = let (genSeed, newOpts) = getGenOpts' xs opts in
    case x of
        HamazeDim (Just dim) -> (genSeed, newOpts { hamazeDimension = dim })
        HamazeSeed (Just seed) -> (False, newOpts { hamazeSeed = mkStdGen seed })
        _ -> error "getGenOpts': validation of parsing was not completed properly!"

generateAndDrawMaze :: HamazeGenOpts -> String
generateAndDrawMaze HamazeGenOpts {hamazeDimension = dim, hamazeSeed = gen} =
    let
        maze = genMazeFromScratch dim gen
        drawn = drawSingleWallMaze maze
    in unlines drawn
        
main :: IO ()
main = do
    args <- getArgs
    case interpretArgs args of
        Left err -> putStrLn err
        Right opts -> do
            answer <- getGenOpts opts
            putStrLn $ generateAndDrawMaze answer
