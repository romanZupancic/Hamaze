module Main where

import System.Environment ( getArgs )
import CmdArgs ( interpretArgs
               , HamazeParse (..)
               )

main :: IO ()
main = do
    args <- getArgs
    print $ interpretArgs args
