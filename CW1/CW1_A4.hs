import Data.List

connected :: [((Integer,Integer),(Integer,Integer))] -> Bool
connected ps = length (isPath ps) <= 1

isPath :: [((Integer,Integer),(Integer,Integer))] -> [[((Integer,Integer),(Integer,Integer))]]
isPath (p:ps) = (p : concat qs) : rs where (qs,rs) = partition (p `intersectionPath`) (isPath ps)
isPath [] = []

-- check the path with other paths one by one
intersectionPath :: ((Integer,Integer),(Integer,Integer)) ->[((Integer,Integer),(Integer,Integer))] -> Bool
intersectionPath p ps = any (intersectPoint p) ps 

-- check the intersection point exists or not
intersectPoint :: ((Integer,Integer),(Integer,Integer))-> ((Integer,Integer),(Integer,Integer)) -> Bool
intersectPoint (p1,p2) (p3,p4) -- p:point
 = (d1 > 0 && d2 < 0 || d1 < 0 && d2 > 0) && (d3 > 0 && d4 < 0 || d3 < 0 && d4 > 0)
  || d1 == 0 && onLine p3 p4 p1 -- p1 on distance (p3,p4)
  || d2 == 0 && onLine p3 p4 p2 -- p2 on distance (p3,p4)
  || d3 == 0 && onLine p1 p2 p3 -- p3 on distance (p1,p2)
  || d4 == 0 && onLine p1 p2 p4 -- p4 on distance (p1,p2)

  where d1 = direction p3 p4 p1
        d2 = direction p3 p4 p2
        d3 = direction p1 p2 p3
        d4 = direction p1 p2 p4

-- Find its direction through vector methods
direction :: (Integer,Integer) -> (Integer,Integer) -> (Integer,Integer) -> Integer
direction q1 q2 q3 = cross_product (pdiff q3 q1) (pdiff q2 q1)
        where
            pdiff (x,y) (x',y') = (x-x',y-y') -- point difference
            cross_product (x,y) (x',y') = x*y'-x'*y --calculate the cross product by vector

-- Check whether the point is between these points or not
-- If not then just return False then not need to check this possibilities
onLine :: (Integer,Integer) -> (Integer,Integer) -> (Integer,Integer) -> Bool
onLine (q1x,q1y) (q2x,q2y) (qx,qy) 
          =  min q1x q2x <= qx
          && qx <= max q1x q2x
          && min q1y q2y <= qy
          && qy <= max q1y q2y

main :: IO()
main = do
    let maze1 = [((0,0),(0,3)), ((0,2),(2,2)), ((2,1),(4,1)), ((4,0),(4,2)), ((4,2),(5,2)), ((2,1),(2,5)),((1,5),(4,5))]
    let maze2 = [((0,0),(0,3)), ((0,2),(2,2)), ((2,1),(4,1)),((4,0),(4,2)), ((4,2),(5,2)), ((2,1),(2,5)),((3,5),(5,5))]

    putStrLn $ "Maze 1 is connected: " ++ show(connected maze1) -- True
    putStrLn $ "Maze 2 is connected: " ++ show(connected maze2) -- False
    print (intersectPoint ((0,0),(0,3)) ((0,2),(2,2))) -- True
    print (intersectPoint ((0,0),(0,3)) ((3,5),(5,5))) -- False
    -- print (direction (0,2) (2,2) (0,0)) -- 4
    -- print (direction (0,2) (2,2) (0,3)) -- -2
    -- print (direction (0,0) (0,3) (0,2)) -- 0
    -- print (direction (0,0) (0,3) (2,2)) -- 6