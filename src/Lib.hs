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

data Grid a= Grid { gridWidth :: Int
                  , gridHeight :: Int
                  , gridContents :: [[GridCell a]]
                  }
    deriving Show

type MazeGrid = Grid MazeCell

generateMazeGrid :: Int -> Int -> MazeGrid
generateMazeGrid x y = Grid x y [[defGridCell xv yv | xv <- [0..(x - 1)]] | yv <- [0..(y - 1)]]
    where
        defGridCell cellx celly = GridCell cellx celly (MazeCell True True True True False)

gridSelect :: Grid a -> Int -> Int -> GridCell a
gridSelect grid x y = (gridContents grid) !! y !! x

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
            | otherwise = error "We are comparing cell locations of the same cell"
        fstReplacement = replaceMaze mazeGrid x y
            (makeNewCell (info $ gridSelect mazeGrid x y) (x - nx) (y - ny))
        sndReplacement = replaceMaze fstReplacement nx ny
            (makeNewCell (info $ gridSelect fstReplacement nx ny) (nx - x) (ny - y))

generateMaze :: MazeGrid -> Int -> Int -> [Int] -> MazeGrid
generateMaze mazeGrid x y neighbourOrder =
  foldr (connectCells) markedVisited neighboursReordered
    where
        currCell = gridSelect mazeGrid x y
        markedVisited = replaceMaze mazeGrid x y ((info currCell) {visited = True})
        (neighboursReordered, nxt) = let
          neighbours = surroundingCoords x y (gridWidth mazeGrid, gridHeight mazeGrid)
          (order, nx) = splitAt (length neighbours) neighbourOrder
          in ((reorder neighbours order), nx)
        connectCells (ix,iy) mze = if visited . info $ gridSelect mze ix iy then
                                       mze
                                   else
                                       generateMaze (demolishWall mze x y ix iy) ix iy nxt

replaceMaze :: MazeGrid -> Int -> Int -> MazeCell -> MazeGrid
replaceMaze mze@Grid { gridContents = ctns } x y ele = mze { gridContents = newGrid}
    where
        newCell = (gridSelect mze x y) { info = ele }
        (rowsBefore, rowsInAfter) = splitAt y ctns
        (colsBefore, colsInAfter) = splitAt x (head $ rowsInAfter)
        newRow = colsBefore ++ (newCell : (tail colsInAfter))
        newGrid = rowsBefore ++ (newRow : (tail rowsInAfter))

randomList :: StdGen -> (Int, Int) -> [Int]
randomList gen range@(start, end) = value : randomList nextGen range
    where
        (value, nextGen) = uniformR (start, end) gen

-- | Reorder items according to some weird algorithm that just kinda
-- works. Don't rely on it for anything but wonky results
reorder :: [a] -> [Int] -> [a]
reorder items [] = items
reorder items (x:xs) = (items !! extractIndex) : reorder remainingItems xs
    where
        extractIndex = x `mod` (length items)
        remainingItems = [ele | (idx, ele) <- zip [0..] items, idx /= extractIndex]
    
data MazeGraphics = MazeGraphics { gridCornerGraphic :: String
                                 , gridHorizWallGraphic :: String
                                 , gridVertiWallGraphic :: String
                                 , gridSpaceGraphic :: String
                                 } 

defaultMazeGraphics :: MazeGraphics
defaultMazeGraphics = MazeGraphics "#" "===" "|" "   "

drawMaze :: MazeGraphics -> MazeGrid -> [String]
drawMaze MazeGraphics{ gridCornerGraphic = gcg
                     , gridHorizWallGraphic = ghwg
                     , gridVertiWallGraphic = gvwg
                     , gridSpaceGraphic = gsg
                     } mazeGrid = foldr1 (++) $ map (drawRow . (map info)) (gridContents mazeGrid)
    where
        drawRow :: [MazeCell] -> [String]
        drawRow row = foldr1 (parallelConcat) $ map (drawCell) row
        drawCell :: MazeCell -> [String]
        drawCell MazeCell { leftWall = lw
                          , topWall = tw
                          , rightWall = rw
                          , bottomWall = bw
                          } = [if tw then gcg ++ ghwg ++ gcg else gcg ++ gsg ++ gcg,
                               (if lw then gvwg else " ") ++ gsg ++ (if rw then gvwg else " "),
                               if bw then gcg ++ ghwg ++ gcg else gcg ++ gsg ++ gcg]

parallelConcat :: [[a]] -> [[a]] -> [[a]]
parallelConcat (v:vs) (u:us) = (v ++ u) : parallelConcat vs us
parallelConcat _ _ = []

test :: (Int, Int) -> IO ()
test (width, height) = putStr $ unlines (drawMaze defaultMazeGraphics $ generateMaze (generateMazeGrid width height) 0 0 (randomList (mkStdGen 6969) (0, 11))) 
