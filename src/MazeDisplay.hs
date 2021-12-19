module MazeDisplay where

import MazeGen

data MazeGraphics = MazeGraphics { gridCornerGraphic    :: String
                                 , gridHorizWallGraphic :: String
                                 , gridVertiWallGraphic :: String
                                 , gridSpaceGraphic     :: String
                                 }

defaultMazeGraphics :: MazeGraphics
defaultMazeGraphics = MazeGraphics "#" "===" "|" "   "

drawMaze :: (MazeCell -> [String]) -> MazeGrid -> [String]
drawMaze drawCell mazeGrid = foldr1 (++) $ trimmedRows
    where
        rows = map (drawRow . (map info)) (gridContents mazeGrid)
        trimmedRows = head rows : (tailEach (tail rows))
        drawRow :: [MazeCell] -> [String]
        drawRow row = foldr1 (parallelConcat) $ trimmedCols
            where
                drawnCells  = map (drawCell) row
                trimmedCols = head drawnCells : (map (tailEach) (tail drawnCells))

tailEach :: [[a]] -> [[a]]
tailEach = map (tail)

drawSingleWallCell :: MazeGraphics -> MazeCell -> [String]
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

drawSingleWallMaze :: MazeGrid -> [String]
drawSingleWallMaze = drawMaze (drawSingleWallCell defaultMazeGraphics)

parallelConcat :: [[a]] -> [[a]] -> [[a]]
parallelConcat (v:vs) (u:us) = (v ++ u) : parallelConcat vs us
parallelConcat vs []         = vs
parallelConcat [] us         = us
