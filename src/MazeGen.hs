module MazeGen where

import System.Random

-- | Holds information specific to generating and representing mazes.
data MazeCell = MazeCell { leftWall   :: Bool
                         , topWall    :: Bool
                         , rightWall  :: Bool
                         , bottomWall :: Bool
                         , visited    :: Bool
                         }
    deriving Show

-- | A single cell in a grid
data GridCell a = GridCell { cellX :: Int -- ^ The x-position of the cell
                           , cellY :: Int -- ^ The y-position of the cell
                           , info  :: a   -- ^ Additional information about the cell
                           }
    deriving Show

-- | A grid, a collection of cells
data Grid a = Grid { gridWidth    :: Int            -- ^ The width of the grid (in cells)
                   , gridHeight   :: Int            -- ^ The height of the grid (in cells)
                   , gridContents :: [[GridCell a]] -- ^ The cells in the grid
                   }
    deriving Show

-- | Type alias for shorthand
type MazeGrid = Grid MazeCell

-- | Generate a blank maze-specific grid
generateMazeGrid :: (Int, Int) -- ^ Width x Height
                 -> MazeGrid   -- ^ Width x Height dimensioned maze grid
generateMazeGrid (x, y) = Grid x y [[defGridCell xv yv | xv <- [0..(x - 1)]] | yv <- [0..(y - 1)]]
    where
        defGridCell cellx celly = GridCell cellx celly (MazeCell True True True True False)

-- | Extract a gridcell from a grid
gridSelect :: Grid a     -- ^ The grid
           -> (Int, Int) -- ^ The x and y coordinates of the desired item
           -> GridCell a -- ^ The desired gridcell
gridSelect grid (x, y) = (gridContents grid) !! y !! x

-- | Deduce the coordinates of the top, bottom, left, and right cells, if they exist
surroundingCoords :: (Int, Int)   -- ^ The x-coordinate of the inspecting cell
                  -> (Int, Int)   -- ^ The (width, height) of the grid the cell exists in
                  -> [(Int, Int)] -- ^ A list of vertical and horizontal immediate neighbours
surroundingCoords (x, y) bounds = [(vx, y) | vx <- approvedX] ++ [(x, vy) | vy <- approvedY]
    where
        approvedX = (if x /= 0 then [x - 1] else []) ++ (if x /= (fst bounds) - 1 then [x + 1] else [])
        approvedY = (if y /= 0 then [y - 1] else []) ++ (if y /= (snd bounds) - 1 then [y + 1] else [])

-- | Remove the wall between two (neighbouring) cells
demolishWall :: MazeGrid   -- ^ The mazegrid to "modify"
             -> (Int, Int) -- ^ The coordinates of the first cell
             -> (Int, Int) -- ^ The coordinates of the second cell
             -> MazeGrid   -- ^ The resulting maze grid
demolishWall mazeGrid one@(x, y) two@(nx, ny) = sndReplacement
    where
        makeNewCell mzeCell dx dy
            | dx > 0 = mzeCell {leftWall = False}
            | dx < 0 = mzeCell {rightWall = False}
            | dy > 0 = mzeCell {topWall = False}
            | dy < 0 = mzeCell {bottomWall = False}
            | otherwise = error "We are comparing cell locations of the same cell"
        fstReplacement = replaceMaze mazeGrid one
            (makeNewCell (info $ gridSelect mazeGrid one) (x - nx) (y - ny))
        sndReplacement = replaceMaze fstReplacement two
            (makeNewCell (info $ gridSelect fstReplacement two) (nx - x) (ny - y))

-- | Generate a maze
generateMaze :: MazeGrid   -- ^ An inital maze to "modify"
             -> (Int, Int) -- ^ The coordinates of the cell whose walls will be removed/traversed across
             -> [Int]      -- ^ A stream of numbers to determine traversal path
             -> MazeGrid   -- ^ A mazed grid
generateMaze mazeGrid (x, y) neighbourOrder =
  foldr (connectCells) markedVisited neighboursReordered
    where
        currCell = gridSelect mazeGrid (x, y)
        markedVisited = replaceMaze mazeGrid (x, y) ((info currCell) {visited = True})
        (neighboursReordered, nxt) = let
          neighbours = surroundingCoords (x, y) (gridWidth mazeGrid, gridHeight mazeGrid)
          (order, nx) = splitAt (length neighbours) neighbourOrder
          in ((reorder neighbours order), nx)
        connectCells (ix,iy) mze = if visited . info $ gridSelect mze (ix, iy) then
                                       mze
                                   else
                                       generateMaze (demolishWall mze (x, y) (ix, iy)) (ix, iy) nxt

-- | Replace the information of a specific cell in a grid
replaceMaze :: Grid a     -- ^ The grid whose cell contents will be replaced
            -> (Int, Int) -- ^ The coordinates of the target cell
            -> a          -- ^ The data to replace with
            -> Grid a     -- ^ The new grid
replaceMaze mze@Grid { gridContents = ctns } (x, y) ele = mze { gridContents = newGrid}
    where
        newCell = (gridSelect mze (x, y)) { info = ele }
        (rowsBefore, rowsInAfter) = splitAt y ctns
        (colsBefore, colsInAfter) = splitAt x (head $ rowsInAfter)
        newRow = colsBefore ++ (newCell : (tail colsInAfter))
        newGrid = rowsBefore ++ (newRow : (tail rowsInAfter))

-- | Generate a list containing randomized elements within a range
genRandomList :: StdGen -- ^ The random generation to be used to generate the numbers
              -> (Int, Int) -- ^ The range of numbers to generate between
              -> [Int] -- ^ The list
genRandomList gen range@(start, end) = value : genRandomList nextGen range
    where
        (value, nextGen) = uniformR (start, end) gen

-- | Reorder items according to some weird algorithm that just kinda
-- works. Don't rely on it for anything but wonky results.
reorder :: [a]   -- ^ The source of the items
        -> [Int] -- ^ Numbers that control how the list is reordered
        -> [a]   -- ^ The reordered list
reorder items [] = items
reorder items (x:xs) = (items !! extractIndex) : reorder remainingItems xs
    where
        extractIndex = x `mod` (length items)
        remainingItems = [ele | (idx, ele) <- zip [0..] items, idx /= extractIndex]
