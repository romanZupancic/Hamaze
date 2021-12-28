module Main where

import System.Environment ( getArgs )
import CmdArgs ( parseArgs )

main :: IO ()
main = do
    args <- getArgs
    parseArgs
