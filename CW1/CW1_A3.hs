import Text.Read (Lexeme(String))
-- CW1 A3
renderMaze :: [((Integer,Integer),(Integer,Integer))] -> [String]
renderMaze [] = []
renderMaze maze = map setRow [0..maxY]
    where
        -- [((x1,y1),(x2,y2))]
        -- find the maximum x and y
        maxX = findMaxX (listMaxX maze)
        maxY = findMaxY (listMaxY maze)
        
        -- list out all the paths from the maze and return it as a list
        isPath :: ((Integer,Integer),(Integer,Integer)) -> [(Integer,Integer)]
        isPath ((x1,y1),(x2,y2)) | x1==x2 = [(x1,y) | y <- [min y1 y2..max y1 y2]]
                                 | y1==y2 = [(x,y1) | x <- [min x1 x2..max x1 x2]]
        
        -- store all the paths in this list
        pathCoords = concatMap isPath maze 

        -- print the coordinates is path with '#'
        setRow :: Integer -> String
        setRow y = [if (x,y) `elem` pathCoords then '#' else ' '| x <- [0..maxX]] 

listMaxX :: [((Integer,Integer),(Integer,Integer))] -> [Integer]
listMaxX [] = []
listMaxX (((x1,y1),(x2,y2)):xs) = x1:x2 : listMaxX xs

findMaxX :: [Integer] -> Integer
findMaxX [x] = x
findMaxX (x:y:xs) = findMaxX (max x y: xs)

listMaxY :: [((Integer,Integer),(Integer,Integer))] -> [Integer]
listMaxY [] = []
listMaxY (((x1,y1),(x2,y2)):xs) = y1:y2 : listMaxY xs

findMaxY :: [Integer] -> Integer
findMaxY [y] = y
findMaxY (y:z:ys) = findMaxY (max y z: ys)

main :: IO()
main = do 
    let maze = [((0,0),(0,3)), ((0,2),(2,2)), ((2,1),(4,1)), ((4,0),(4,2)), ((4,2),(5,2)), ((2,1),(2,5)),((1,5),(4,5))]
    putStrLn $ "Max x is " ++ show (findMaxX (listMaxX maze)) -- 5
    putStrLn $ "Max y is " ++ show (findMaxY (listMaxY maze)) -- 5
    mapM_ putStrLn (renderMaze maze)
