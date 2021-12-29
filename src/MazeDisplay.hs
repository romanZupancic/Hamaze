module MazeDisplay ( drawSingleWallMaze ) where

import MazeGen

-- | How to display various features of a single maze cell
data MazeGraphics = MazeGraphics { gridCornerGraphic    :: String
                                 , gridHorizWallGraphic :: String
                                 , gridVertiWallGraphic :: String
                                 , gridSpaceGraphic     :: String
                                 }

-- | A default supplied maze graphic
defaultMazeGraphics :: MazeGraphics
defaultMazeGraphics = MazeGraphics "#" "===" "|" "   "

-- | Draw a maze. The maze is "drawn" into a list of strings, where
-- each string is a new visual row (not cellular row) of the maze.
drawMaze :: (MazeCell -> [String]) -- ^ The function which converts maze cells to lists of string
         -> MazeGrid               -- ^ The MazeGrid to draw
         -> [String]               -- ^ The output drawing
drawMaze drawCell mazeGrid = foldr1 (++) $ trimmedRows
    where
        rows = map (drawRow . (map info)) (gridContents mazeGrid)
        trimmedRows = head rows : (tailEach (tail rows))
        drawRow :: [MazeCell] -> [String]
        drawRow row = foldr1 (parallelConcat) $ trimmedCols
            where
                drawnCells  = map (drawCell) row
                trimmedCols = head drawnCells : (map (tailEach) (tail drawnCells))

-- | A helper function which extracts the tail of lists in a list
tailEach :: [[a]] -> [[a]]
tailEach = map (tail)

-- | Draw a cell with a single wall separating it from other cells
drawSingleWallCell :: MazeGraphics -- ^ The graphics to use to represent parts of the cell
                   -> MazeCell     -- ^ The cell to convert
                   -> [String]     -- ^ The output
drawSingleWallCell MazeGraphics{ gridCornerGraphic = gcg
                     , gridHorizWallGraphic = ghwg
                     , gridVertiWallGraphic = gvwg
                     , gridSpaceGraphic = gsg
                     }
               MazeCell { leftWall = lw
                        , topWall = tw
                        , rightWall = rw
                        , bottomWall = bw
                        } = [if tw then gcg ++ ghwg ++ gcg else gcg ++ gsg ++ gcg,
                            (if lw then gvwg else " ") ++ gsg ++ (if rw then gvwg else " "),
                            if bw then gcg ++ ghwg ++ gcg else gcg ++ gsg ++ gcg]

-- | A helper function which draws a maze with a single wall and
-- hard-coded graphics defaults
drawSingleWallMaze :: MazeGrid -> [String]
drawSingleWallMaze = drawMaze (drawSingleWallCell defaultMazeGraphics)

-- | Concatenate lists of lists together.
-- Each list will be concatenated with the corresponding list in it's "level".
-- If the input double lists are of different lengths, the extra
-- elements are concatenated unmodified.
parallelConcat :: [[a]] -> [[a]] -> [[a]]
parallelConcat (v:vs) (u:us) = (v ++ u) : parallelConcat vs us
parallelConcat vs []         = vs
parallelConcat [] us         = us
