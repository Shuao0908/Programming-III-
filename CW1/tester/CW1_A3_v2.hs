-- This module, named 'MazeRenderer', contains a function to render a maze row-by-row as a list of strings.
-- Each character in each string represents a coordinate within the maze. If a path is present at that coordinate,
-- the character should be a hash symbol '#'. If a path is not present, the character should be a space.
 
module MazeRenderer where
 
-- This function takes a list of pairs of coordinates representing the ends of paths in the maze,
-- and renders the maze row-by-row as a list of strings.
--
-- Parameters:
--  * `paths`: A list of pairs of coordinates representing the ends of paths in the maze.
--
-- Returns:
--  * A list of strings, where each string represents a row in the rendered maze.
--
-- Exceptions:
--  * None. However, note that if the given list of paths is empty, the resulting maze will also be empty.
--
renderMaze :: [((Int, Int), (Int, Int))] -> [String]
renderMaze paths = 
    let maxX = maximum (map (maximum . map fst) paths)  -- Find the maximum x-coordinate
        maxY = maximum (map (maximum . map snd) paths)  -- Find the maximum y-coordinate
        maze = [ [ if ((x1 == x2 && y == y1) || (y1 == y2 && x == x1)) then '#' else ' ' | x <- [0..maxX] ] | y <- [0..maxY] ]  -- Create an empty maze with spaces
    in foldl (\m ((x1, y1), (x2, y2)) -> updateMaze m x1 y1 x2 y2) maze paths  -- Update the maze with the paths
 
-- This helper function updates the maze by replacing the spaces with hash symbols '#' at the coordinates
-- specified by the given path.
--
-- Parameters:
--  * `maze`: The current state of the maze.
--  * `x1`, `y1`, `x2`, `y2`: The coordinates of the path.
--
-- Returns:
--  * The updated maze with the path represented by hash symbols '#'.
--
-- Exceptions:
--  * None. However, note that if the given coordinates are out of bounds of the maze, the function will throw an error.
--
updateMaze :: [String] -> Int -> Int -> Int -> Int -> [String]
updateMaze maze x1 y1 x2 y2 =
    let pathChar = '#'  -- Character to represent the path
        updatedRow = updateRow (maze !! y1) x1 x2 pathChar  -- Update the row containing the starting point of the path
        updatedMaze = take y1 maze ++ [updatedRow] ++ drop (y1 + 1) maze  -- Update the maze with the updated row
    in if y1 == y2  -- If the path is horizontal
        then updatedMaze
        else updateMaze updatedMaze y1 x1 y2 x2  -- If the path is vertical, swap the coordinates and update again
 
-- This helper function updates a row in the maze by replacing the characters between the given x-coordinates
-- with the specified character.
--
-- Parameters:
--  * `row`: The current state of the row.
--  * `x1`, `x2`: The x-coordinates of the path.
--  * `char`: The character to replace the existing characters with.
--
-- Returns:
--  * The updated row with the path represented by the specified character.
--
-- Exceptions:
--  * None. However, note that if the given coordinates are out of bounds of the row, the function will throw an error.
--
updateRow :: String -> Int -> Int -> Char -> String
updateRow row x1 x2 char =
    let updatedRow = take x1 row ++ replicate (x2 - x1 + 1) char ++ drop (x2 + 1) row  -- Update the characters between the given x-coordinates
    in updatedRow
 
-- This function demonstrates an example of rendering a maze with paths.
--
-- Exceptions:
--  * None, given the constraints of the used functions. 
--
demonstrateExample :: IO ()
demonstrateExample = do
    -- Example: Render a maze with two paths: (0,0) to (2,0) and (1,1) to (1,3)
    putStrLn "Example: renderMaze [((0,0), (2,0)), ((1,1), (1,3))]"
    let maze = renderMaze [((0,0), (2,0)), ((1,1), (1,3))]
    mapM_ putStrLn maze


