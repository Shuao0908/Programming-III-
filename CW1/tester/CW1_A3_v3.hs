
renderMaze :: [((Integer, Integer), (Integer, Integer))] -> [String]
renderMaze paths = map renderRow [0..maxY]
  where
    maxX = maximum $ concatMap (\((x1, _), (x2, _)) -> [x1, x2]) paths
    maxY = maximum $ concatMap (\((_, y1), (_, y2)) -> [y1, y2]) paths

    isPath (x, y) = any (\((x1, y1), (x2, y2)) ->
                        (x1 == x && y1 <= y && y <= y2) ||
                        (x2 == x && y1 <= y && y <= y2) ||
                        (y1 == y && x1 <= x && x <= x2) ||
                        (y2 == y && x1 <= x && x <= x2)) paths

    renderRow y = [if isPath (x, y) then '#' else ' ' | x <- [0..maxX]]

-- Example usage:
maze = [((0,0),(0,3)), ((0,2),(2,2)), ((2,1),(4,1)),
        ((4,0),(4,2)), ((4,2),(5,2)), ((2,1),(2,5)),
        ((1,5),(4,5))]

renderedMaze = renderMaze maze

main = do 
  mapM_ putStrLn renderedMaze
