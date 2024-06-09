-- CW2
-- (col,row)
type Pos =(Int,Int)

data Side = North | East | South | West deriving (Show,Eq,Ord)

-- eg (North, 5)
type EdgePos = (Side,Int)

type Atoms = [Pos]

type Interactions = [(EdgePos, Marking)]

data Marking = Absorb | Reflect | Path EdgePos deriving (Show,Eq)

-- return the set of interactions from all possible edge entry point
calcBBInteractions :: Int -> Atoms -> IO()--Interactions
calcBBInteractions num atomsCoord = do
    putStrLn "Atoms Coord:"
    print atomsCoord
    let reflectCoord = detectReflectCoord atomsCoord
    putStrLn "Reflected Coord:"
    print reflectCoord
    let coordInMap = generateAllCoordInMap num
    putStrLn "All Coord in Map:"
    print coordInMap

    let northCoord = findNorthCoord num
    putStrLn "North Coord:"
    print northCoord
    let finalPathNorth = pathFromNorthCoord northCoord atomsCoord num
    putStrLn "Final Coord:"
    print finalPathNorth

    let eastCoord = findEastCoord num
    putStrLn "East Coord:"
    print eastCoord
    let finalPathEast = pathFromEastCoord eastCoord atomsCoord num
    putStrLn "Final Coord:"
    print finalPathEast

    let southCoord = findSouthCoord num
    putStrLn "South Coord:"
    print southCoord
    let finalPathSouth = pathFromSouthCoord southCoord atomsCoord num
    putStrLn "Final Coord:"
    print finalPathSouth

    let westCoord = findWestCoord num
    putStrLn "West Coord:"
    print westCoord
    let finalPathWest = pathFromWestCoord westCoord atomsCoord num
    putStrLn "Final Coord:"
    print finalPathWest


-- generate all the coordinates based on the size of North
generateAllCoordInMap :: Int -> Atoms
generateAllCoordInMap n = [(x,y) | x<-[1..n],y<-[1..n]]

-- detect the top-left, top-right, bottom-left, bottom-right of the atoms and store it in the list
detectReflectCoord :: Atoms -> Atoms
detectReflectCoord [] = []
detectReflectCoord ((x,y):xs) = (x-1,y-1):(x-1,y+1):(x+1,y-1):(x+1,y+1):detectReflectCoord xs

-- detect the top-right reflect coord
detectTopRightCoord :: Atoms -> Atoms
detectTopRightCoord [] = []
detectTopRightCoord ((x,y):xs) = (x+1,y-1):detectTopRightCoord xs

-- detect the top-left reflect coord
detectTopLeftCoord :: Atoms -> Atoms
detectTopLeftCoord [] = []
detectTopLeftCoord ((x,y):xs) = (x-1,y-1):detectTopLeftCoord xs

-- detect the bottom-right reflect coord
detectBottomRightCoord :: Atoms -> Atoms
detectBottomRightCoord [] = []
detectBottomRightCoord ((x,y):xs) = (x+1,y+1):detectBottomRightCoord xs

-- detect the bottom-left reflect coord
detectBottomLeftCoord :: Atoms -> Atoms
detectBottomLeftCoord [] = []
detectBottomLeftCoord ((x,y):xs) = (x-1,y+1):detectBottomLeftCoord xs

-- find all north coordinates in the map
findNorthCoord :: Int -> Atoms
findNorthCoord n = [(x,y) | x<-[1..n] ,y<-[0]]

-- find all east coordinates in the map
findEastCoord :: Int -> Atoms
findEastCoord n = [(x,y) | x<-[n+1] ,y<-[1..n]]

-- find all south coordinates in the map
findSouthCoord :: Int -> Atoms
findSouthCoord n = [(x,y)| x<-[1..n],y<-[n+1]]

-- find all west coordinates in the map
findWestCoord :: Int -> Atoms
findWestCoord n = [(x,y) | x<-[0] ,y<-[1..n]]

pathFromNorthCoord :: Atoms-> Atoms -> Int -> Atoms
pathFromNorthCoord xs atomsCoord num = map (\ x -> goVerticalPath num x 1 atomsCoord) xs

pathFromEastCoord :: Atoms-> Atoms -> Int -> Atoms
pathFromEastCoord xs atomsCoord num = map (\ x -> goHorizontalPath num x (-1) atomsCoord) xs

pathFromSouthCoord :: Atoms-> Atoms -> Int -> Atoms
pathFromSouthCoord xs atomsCoord num = map (\ x -> goVerticalPath num x (-1) atomsCoord) xs

pathFromWestCoord :: Atoms-> Atoms -> Int -> Atoms
pathFromWestCoord xs atomsCoord num = map (\ x -> goHorizontalPath num x 1 atomsCoord) xs

goVerticalPath :: Int -> Pos -> Int  -> Atoms -> Pos
goVerticalPath num (x,y) direction atomsCoord
    | (x,y) `elem` atomsCoord = (x,y)

    | elem (x,y) topLeftCornerAtom && x>0 = goHorizontalPath num (x-1,y) (-1) atomsCoord-- move left to west direction (-1)
    | elem (x,y) topLeftCornerAtom && x==0 = (x,y)

    | elem (x,y) topRightCornerAtom && x<(num+1) = goHorizontalPath num (x+1,y) 1 atomsCoord -- move right to east direction (+1)
    | elem (x,y) topRightCornerAtom && x==num+1 = (x,y)

    | elem (x,y) bottomLeftCornerAtom && x>0 = goHorizontalPath num (x-1,y) (-1) atomsCoord -- move left to west direction (-1)
    | elem (x,y) bottomLeftCornerAtom && x==0 = (x,y)

    | elem (x,y) bottomRightCornerAtom && x<num+1 = goHorizontalPath num (x+1,y) 1 atomsCoord -- move right to east direction (+1)
    | elem (x,y) bottomRightCornerAtom && x==num+1 = (x,y)

    | y>0 && direction==(-1) = goVerticalPath num (x,y-1) (-1) atomsCoord
    | y<(num+1) && direction==1 = goVerticalPath num (x,y+1) 1  atomsCoord

    | y==(num+1) || y==0 = (x,y)
        where
            topLeftCornerAtom = detectTopLeftCoord atomsCoord
            topRightCornerAtom = detectTopRightCoord atomsCoord
            bottomLeftCornerAtom = detectBottomLeftCoord atomsCoord
            bottomRightCornerAtom = detectBottomRightCoord atomsCoord

goHorizontalPath:: Int -> Pos -> Int -> Atoms -> Pos
goHorizontalPath num (x,y) direction atomsCoord
    | (x,y) `elem` atomsCoord = (x,y)

    | elem (x,y) topLeftCornerAtom && y>0 = goVerticalPath num (x,y-1) (-1) atomsCoord-- move upward to north direction (-1)
    | elem (x,y) topLeftCornerAtom && y==0 = (x,y)

    | elem (x,y) topRightCornerAtom && y<num+1 = goVerticalPath num (x,y-1) (-1) atomsCoord -- move upward to north direction (-1)
    | elem (x,y) topRightCornerAtom && y==num+1 = (x,y)

    | elem (x,y) bottomLeftCornerAtom && y>0 = goVerticalPath num (x,y+1) 1 atomsCoord -- move downward to west direction (+1)
    | elem (x,y) bottomLeftCornerAtom && y==0 = (x,y)

    | elem (x,y) bottomRightCornerAtom && y<num+1 = goVerticalPath num (x,y+1) 1 atomsCoord -- move downward to east direction (+1)
    | elem (x,y) bottomRightCornerAtom && y==num+1 = (x,y)

    | x>0 && direction==(-1) = goHorizontalPath num (x-1,y) (-1) atomsCoord
    | x<num+1 && direction==1 = goHorizontalPath num (x+1,y) 1 atomsCoord

    | x==num+1 || x==0 = (x,y)
        where
            topLeftCornerAtom = detectTopLeftCoord atomsCoord
            topRightCornerAtom = detectTopRightCoord atomsCoord
            bottomLeftCornerAtom = detectBottomLeftCoord atomsCoord
            bottomRightCornerAtom = detectBottomRightCoord atomsCoord
-- return a list of the positions of exactly N atoms that gives rise to the given list of interactions
-- solveBB :: Int -> Interactions -> Atoms

main :: IO ()
main = calcBBInteractions 8 [(2,3),(7,3),(4,6),(7,8)]