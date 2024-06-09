import Data.List (nub)

connected :: [((Integer, Integer), (Integer, Integer))] -> Bool
connected [] = True  -- An empty maze is considered connected
connected maze =
    let pathCoords = nub $ concatMap isPath maze
    in isConnected pathCoords maze

-- Given a list of path coordinates, check if all paths are connected
isConnected :: [(Integer, Integer)] -> [((Integer, Integer), (Integer, Integer))] -> Bool
isConnected [] _ = True
isConnected (coord:rest) maze =
    let reachablePaths = filter (hasCoord coord) maze
    in if null reachablePaths
        then False  -- This coordinate is not on any path, so the maze is not connected
        else isConnected rest (filter (/= head reachablePaths) maze)

-- Check if a coordinate is part of a path
hasCoord :: (Integer, Integer) -> ((Integer, Integer), (Integer, Integer)) -> Bool
hasCoord coord ((x1, y1), (x2, y2)) =
    (x1 == x2 && x1 == coord) && (y1 <= coord && coord <= y2) ||
    (y1 == y2 && y1 == coord) && (x1 <= coord && coord <= x2)

-- Extract the coordinates along a path
isPath :: ((Integer, Integer), (Integer, Integer)) -> [(Integer, Integer)]
isPath ((x1, y1), (x2, y2))
    | x1 == x2 = [(x1, y) | y <- [min y1 y2..max y1 y2]]
    | y1 == y2 = [(x, y1) | x <- [min x1 x2..max x1 x2]]

-- Example usage:
maze = [((0,0),(0,3)), ((0,2),(2,2)), ((2,1),(4,1)),
        ((4,0),(4,2)), ((4,2),(5,2)), ((2,1),(2,5)),
        ((1,5),(4,5))]

isMazeConnected = connected maze

