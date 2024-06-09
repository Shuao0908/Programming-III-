{-# LANGUAGE ParallelListComp #-}
import Data.List
isPath :: ((Integer,Integer),(Integer,Integer)) -> [(Integer,Integer)]
isPath ((x1,y1),(x2,y2)) | x1==x2 = [(x1,y) | y <- [min y1 y2..max y1 y2]]
                         | y1==y2 = [(x,y1) | x <- [min x1 x2..max x1 x2]]

-- print the coordinates is path with '#'
-- checkRow :: [(Integer,Integer)]-> [(Integer,Integer)] -> [(Integer,Integer)]
-- checkRow [] = False
-- checkRow ((x,y):xs) list = if length.filter(==(x,y)) list >1 then ((x,y) checkRow ((x,y):xs) list) else error "Cannot Do"


type Path = ((Integer, Integer), (Integer, Integer))

intersects :: Path -> Path -> Bool
intersects (a1, b1) (a2, b2) = a1 == a2 && b1 == b2 || a1 == a2 && b1 /= b2 || a1 /= a2 && b1 == b2 || a1 /= a2 && b1 /= b2

connected :: [Path] -> Bool
connected paths = not $ any (null . (\path -> filter (intersects path) paths)) paths
coordRecord = [(x,y) | x<-[0..5] |y<-[0..5]]

-- type Coordinate = (Integer, Integer)
-- type Path = (Coordinate, Coordinate)

-- connected2 :: [Path] -> Bool
-- connected2 [] = True
-- connected2 paths = any (not . unique) [minX, maxX, minY, maxY]
--     where
--         (minX, maxX) = foldl (\(min, max) ((x1, _), (x2, _))) (min, max) paths
--         (minY, maxY) = foldl (\(min, max) ((_, y1), (_, y2))) (min, max) paths
--         unique value = length (filter (== value) [minX, maxX, minY, maxY]) == 1

-- type Coordinate = (Integer, Integer)
-- type Path = (Coordinate, Coordinate)

connected2 :: [Path] -> Bool
connected2 [] = True
connected2 paths = any (not . unique) [minX, maxX, minY, maxY]
    where
        (minX, maxX) = foldl (\(minX, maxX) ((x1, _), (x2, _)) -> (min x1 minX, max x2 maxX)) (maxBound, minBound) paths
        (minY, maxY) = foldl (\(minY, maxY) ((_, y1), (_, y2)) -> (min y1 minY, max y2 maxY)) (maxBound, minBound) paths
        unique value = length (filter (== value) [minX, maxX, minY, maxY]) == 1

main =do
    let maze = [((0,0),(0,3)), ((0,2),(2,2)), ((2,1),(4,1)), ((4,0),(4,2)), ((4,2),(5,2)), ((2,1),(2,5)),((1,5),(4,5))]
    let maze2 = [((0,0),(0,3)), ((0,2),(2,2)), ((2,1),(4,1)),((4,0),(4,2)), ((4,2),(5,2)), ((2,1),(2,5)),((3,5),(5,5))]
    print(isPath ((0,0),(0,3))) --[(0,0),(0,1),(0,2),(0,3)]
    print(isPath ((0,2),(2,2))) --[(0,2),(1,2),(2,2)]
    print(isPath ((2,1),(4,1))) --[(2,1),(3,1),(4,1)]
    print(isPath ((4,0),(4,2))) --[(4,0),(4,1),(4,2)]
    print(isPath ((4,2),(5,2))) --[(4,2),(5,2)]
    print(isPath ((2,1),(2,5))) --[(2,1),(2,2),(2,3),(2,4),(2,5)]
    print(isPath ((1,5),(4,5))) --[(1,5),(2,5),(3,5),(4,5)]
    print(map isPath maze) 
    print(concatMap isPath maze)
    print (connected maze)
    print (connected maze2)
    print (connected2 maze)
    print (connected2 maze2)