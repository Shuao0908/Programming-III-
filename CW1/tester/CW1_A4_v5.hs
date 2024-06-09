import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List (foldl')
import Data.Maybe (fromMaybe)

type Point = (Integer, Integer)
type Path = (Point, Point)
type Graph = Map.Map Point (Set.Set Point)

-- Add a path to the graph
addPath :: Graph -> Path -> Graph
addPath graph ((x1, y1), (x2, y2))
  | x1 == x2 = foldl' (\acc y -> connectPoints acc (x1, y) (x1, y+1)) graph [min y1 y2 .. max y1 y2 - 1]
  | y1 == y2 = foldl' (\acc x -> connectPoints acc (x, y1) (x+1, y1)) graph [min x1 x2 .. max x1 x2 - 1]
  | otherwise = graph  -- This should never happen for horizontal/vertical paths

-- Connect two points in the graph
connectPoints :: Graph -> Point -> Point -> Graph
connectPoints graph p1 p2 =
  let graphWithP1 = Map.insertWith Set.union p1 (Set.singleton p2) graph
      graphWithP2 = Map.insertWith Set.union p2 (Set.singleton p1) graphWithP1
  in graphWithP2

-- Depth-First Search to check for connectedness
dfs :: Set.Set Point -> [Point] -> Graph -> Set.Set Point
dfs visited [] _ = visited
dfs visited (x:xs) graph =
    let newVisited = Set.insert x visited
        newNeighbors = fromMaybe Set.empty (Map.lookup x graph)
        newToVisit = Set.toList (newNeighbors `Set.difference` newVisited) ++ xs
    in dfs newVisited newToVisit graph

-- Collect all points including intermediate ones
allPoints :: [Path] -> Set.Set Point
allPoints [] = Set.empty
allPoints paths = foldl' (\acc ((x1, y1), (x2, y2)) ->
                          if x1 == x2
                          then foldl' (\acc' y -> Set.insert (x1, y) acc') acc [min y1 y2 .. max y1 y2]
                          else foldl' (\acc' x -> Set.insert (x, y1) acc') acc [min x1 x2 .. max x1 x2]
                        ) Set.empty paths

-- Now, update the connected function to use the new allPoints function
connected :: [Path] -> Bool
connected [] = True -- An empty maze is considered connected
connected paths =
  let graph = foldl' addPath Map.empty paths
      points = allPoints paths
      startingPoint = fst (head paths)
      visited = dfs Set.empty [startingPoint] graph
  in Set.size visited == Set.size points

-- Example usage:
maze1 :: [Path]
maze1 = [((0,0),(0,3)), ((0,2),(2,2)), ((2,1),(4,1)),
         ((4,0),(4,2)), ((4,2),(5,2)), ((2,1),(2,5)),
         ((1,5),(4,5))]


maze2 :: [Path]
maze2 = [((0,0),(0,3)), ((0,2),(2,2)), ((2,1),(4,1)),
         ((4,0),(4,2)), ((4,2),(5,2)), ((2,1),(2,5)),
         ((3,5),(5,5))]

main :: IO ()
main = do
  putStrLn $ "Maze 1 is connected: " ++ show (connected maze1)
  putStrLn $ "Maze 2 is connected: " ++ show (connected maze2)