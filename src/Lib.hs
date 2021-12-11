{-# OPTIONS_GHC -Wall #-}

module Lib where

import System.Random

data MazeCell = MazeCell { leftWall :: Bool
                         , topWall :: Bool
                         , rightWall :: Bool
                         , bottomWall :: Bool
                         , visited :: Bool
                         }
    deriving Show

data GridCell a = GridCell { cellX :: Int
                           , cellY :: Int
                           , info :: a
                           }
    deriving Show

data Grid a= Grid { width :: Int
                   , height :: Int
                   , contents :: [[GridCell a]]
                   }
    deriving Show

type MazeGrid = Grid MazeCell

generateMazeGrid :: Int -> Int -> MazeGrid
generateMazeGrid x y = Grid x y [[defGridCell xv yv | xv <- [0..(x - 1)]] | yv <- [0..(y - 1)]]
    where
        defGridCell cellx celly = GridCell cellx celly (MazeCell True True True True False)

gridSelect :: Grid a -> Int -> Int -> GridCell a
gridSelect grid x y = (contents grid) !! y !! x

surroundingCoords :: Int -> Int -> (Int, Int) -> [(Int, Int)]
surroundingCoords x y bounds = [(vx, y) | vx <- approvedX] ++ [(x, vy) | vy <- approvedY]
    where
        approvedX = (if x /= 0 then [x - 1] else []) ++ (if x /= (fst bounds) - 1 then [x + 1] else [])
        approvedY = (if y /= 0 then [y - 1] else []) ++ (if y /= (snd bounds) - 1 then [y + 1] else [])

demolishWall :: MazeGrid -> Int -> Int -> Int -> Int -> MazeGrid
demolishWall mazeGrid x y nx ny = sndReplacement
    where
        makeNewCell mzeCell dx dy
            | dx > 0 = mzeCell {leftWall = False}
            | dx < 0 = mzeCell {rightWall = False}
            | dy > 0 = mzeCell {topWall = False}
            | dy < 0 = mzeCell {bottomWall = False}
        fstReplacement = replaceMaze mazeGrid x y
            (makeNewCell (info $ gridSelect mazeGrid x y) (x - nx) (y - ny))
        sndReplacement = replaceMaze fstReplacement nx ny
            (makeNewCell (info $ gridSelect fstReplacement nx ny) (nx - x) (ny - y))
    
generateMaze :: MazeGrid -> Int -> Int -> MazeGrid
generateMaze mazeGrid x y =
  foldr (connectCells) markedVisited neighbours
    where
        currCell = gridSelect mazeGrid x y
        markedVisited = replaceMaze mazeGrid x y ((info currCell) {visited = True})
        neighbours = surroundingCoords x y (width mazeGrid, height mazeGrid)
        connectCells (ix,iy) mze = if visited . info $ gridSelect mze ix iy then
                                       mze
                                   else
                                       generateMaze (demolishWall mze x y ix iy) ix iy

replaceMaze :: MazeGrid -> Int -> Int -> MazeCell -> MazeGrid
replaceMaze mze@Grid { contents = ctns } x y ele =
    mze { contents = [map (replacer x y) col | col <- ctns] }
    where
        replacer tx ty cell@GridCell{cellX = cx, cellY = cy}
            = if tx == cx && ty == cy then
                  cell {info = ele}
              else
                  cell

randomList :: StdGen -> (Int, Int) -> [Int]
randomList gen range@(start, end) = value : randomList nextGen range
    where
        (value, nextGen) = uniformR (start, end) gen

data MazeGraphics = MazeGraphics { gridCornerGraphic :: Char
                                 , gridHorizWallGraphic :: Char
                                 , gridVertiWallGraphic :: Char
                                 } 

defaultMazeGraphics :: MazeGraphics
defaultMazeGraphics = MazeGraphics '#' '=' '|'

drawMaze :: MazeGraphics -> MazeGrid -> [String]
drawMaze MazeGraphics{ gridCornerGraphic = gcg
                     , gridHorizWallGraphic = ghwg
                     , gridVertiWallGraphic = gvwg
                     } mazeGrid = foldr1 (++) $ map (drawRow . (map info)) (contents mazeGrid)
    where
        drawRow :: [MazeCell] -> [String]
        drawRow row = foldr1 (parallelConcat) $ map (drawCell) row
        drawCell :: MazeCell -> [String]
        drawCell MazeCell { leftWall = lw
                          , topWall = tw
                          , rightWall = rw
                          , bottomWall = bw
                          } = [if tw then [gcg, ghwg, gcg] else [gcg, ' ', gcg],
                                    (if lw then gvwg else ' ') : ' ' : (if rw then gvwg else ' ') : [],
                                    if bw then [gcg, ghwg, gcg] else [gcg, ' ', gcg]]

parallelConcat :: [[a]] -> [[a]] -> [[a]]
parallelConcat (v:vs) (u:us) = (v ++ u) : parallelConcat vs us
parallelConcat _ _ = []

test :: IO ()
test = putStr $ unlines (drawMaze defaultMazeGraphics $ generateMaze (generateMazeGrid 5 5) 0 0)
