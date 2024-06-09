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
calcBBInteractions :: Int -> Atoms -> Interactions
calcBBInteractions num atomsCoord = finalPathNorth++finalPathEast++finalPathSouth++finalPathWest
        where
            finalPathNorth = pathFromNorthCoord [(x,y) | x<-[1..num] ,y<-[0]] atomsCoord num
            finalPathEast = pathFromEastCoord [(x,y) | x<-[num+1] ,y<-[1..num]] atomsCoord num
            finalPathSouth = pathFromSouthCoord [(x,y)| x<-[1..num],y<-[num+1]] atomsCoord num
            finalPathWest = pathFromWestCoord [(x,y) | x<-[0] ,y<-[1..num]] atomsCoord num 

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

pathFromNorthCoord :: Atoms-> Atoms -> Int -> Interactions
pathFromNorthCoord xs atomsCoord num = map (\ x -> goVerticalPath (North,fst x) num x 1 atomsCoord) xs

pathFromEastCoord :: Atoms-> Atoms -> Int -> Interactions
pathFromEastCoord xs atomsCoord num = map (\ x -> goHorizontalPath (East,snd x) num x (-1) atomsCoord) xs

pathFromSouthCoord :: Atoms-> Atoms -> Int -> Interactions
pathFromSouthCoord xs atomsCoord num = map (\ x -> goVerticalPath (South,fst x) num x (-1) atomsCoord) xs

pathFromWestCoord :: Atoms-> Atoms -> Int -> Interactions
pathFromWestCoord xs atomsCoord num = map (\ x -> goHorizontalPath (West,snd x) num x 1 atomsCoord) xs

goVerticalPath :: EdgePos -> Int -> Pos -> Int  -> Atoms -> (EdgePos,Marking)
goVerticalPath start num (x,y) direction atomsCoord
    -- | (x,y) `elem` atomsCoord = (start,Absorb)
    | elem (x,y) topLeftCornerAtom && y==0 && direction==1= (start,Reflect)
    | elem (x,y) topRightCornerAtom && y==0 && direction==1 = (start,Reflect)
    | elem (x,y) bottomLeftCornerAtom && y==num+1 && direction==(-1) = (start,Reflect)
    | elem (x,y) bottomRightCornerAtom && y==num+1 && direction==(-1) = (start,Reflect)

    | (x,y) `elem` atomsCoord = (start,Absorb)

    | elem (x,y) topLeftCornerAtom && x>0 = goHorizontalPath start num (x-1,y) (-1) atomsCoord-- move left to west direction (-1)          
    | elem (x,y) topRightCornerAtom && x<(num+1) = goHorizontalPath start num (x+1,y) 1 atomsCoord -- move right to east direction (+1)
    | elem (x,y) bottomLeftCornerAtom && x>0 = goHorizontalPath start num (x-1,y) (-1) atomsCoord -- move left to west direction (-1)
    | elem (x,y) bottomRightCornerAtom && x<num+1 = goHorizontalPath start num (x+1,y) 1 atomsCoord -- move right to east direction (+1)
    -- | elem (x,y) topLeftCornerAtom  = goHorizontalPath start num (x-1,y) (-1) atomsCoord-- move left to west direction (-1)          
    -- | elem (x,y) topRightCornerAtom = goHorizontalPath start num (x+1,y) 1 atomsCoord -- move right to east direction (+1)
    -- | elem (x,y) bottomLeftCornerAtom = goHorizontalPath start num (x-1,y) (-1) atomsCoord -- move left to west direction (-1)
    -- | elem (x,y) bottomRightCornerAtom = goHorizontalPath start num (x+1,y) 1 atomsCoord -- move right to east direction (+1)
    | y>=(num+1) && direction==1= (start,Path (South,x))
    | y<=0 && direction==(-1)= (start,Path (North,x))
    -- | y>0 && direction==(-1) = goVerticalPath start num (x,y-1) (-1) atomsCoord
    -- | y<(num+1) && direction==1 = goVerticalPath start num (x,y+1) 1  atomsCoord
    
    -- | (elem (x,y) topLeftCornerAtom || elem (x,y) topRightCornerAtom) && y==0 = (start,Reflect)
    -- | (elem (x,y) bottomLeftCornerAtom || elem (x,y) bottomRightCornerAtom) && y==num+1 = (start,Reflect)
    -- | y==(num+1) && direction==1 = (start,Path(South,x)) -- reach the South side
    -- | y==0 && direction==(-1) = (start,Path(North,x)) -- reach the North Side
    -- | (elem (x,y) topLeftCornerAtom || elem (x,y) bottomLeftCornerAtom) && x>0 = goHorizontalPath start num (x-1,y) (-1) atomsCoord-- move left to west direction (-1)
    -- | (elem (x,y) topRightCornerAtom || elem (x,y) bottomRightCornerAtom) && x<(num+1) = goHorizontalPath start num (x+1,y) 1 atomsCoord -- move right to east direction (+1)
   
    -- | y>0 && direction==(-1) = goVerticalPath start num (x,y-1) (-1) atomsCoord -- move down to south direction (-1)
    -- | y<(num+1) && direction==1 = goVerticalPath start num (x,y+1) 1  atomsCoord -- move up to north direction (+1)
    | otherwise = goVerticalPath start num (x,y+direction) direction atomsCoord
        where
            topLeftCornerAtom = detectTopLeftCoord atomsCoord
            topRightCornerAtom = detectTopRightCoord atomsCoord
            bottomLeftCornerAtom = detectBottomLeftCoord atomsCoord
            bottomRightCornerAtom = detectBottomRightCoord atomsCoord

goHorizontalPath:: EdgePos -> Int -> Pos -> Int -> Atoms -> (EdgePos,Marking)
goHorizontalPath start num (x,y) direction atomsCoord
    | (x,y) `elem` atomsCoord = (start,Absorb)
    | elem (x,y) topLeftCornerAtom && x==0 && direction== 1 = (start,Reflect)
    | elem (x,y) topRightCornerAtom && x==num+1 && direction==(-1) = (start,Reflect)
    | elem (x,y) bottomLeftCornerAtom && x==0 && direction==1 = (start,Reflect)
    | elem (x,y) bottomRightCornerAtom && x==num+1 && direction==(-1) = (start,Reflect)


    | elem (x,y) topLeftCornerAtom && y>0 = goVerticalPath start num (x,y-1) (-1) atomsCoord-- move upward to north direction (-1)
    | elem (x,y) topRightCornerAtom && y<num+1 = goVerticalPath start num (x,y-1) (-1) atomsCoord -- move upward to north direction (-1)
    | elem (x,y) bottomLeftCornerAtom && y>0 = goVerticalPath start num (x,y+1) 1 atomsCoord -- move downward to west direction (+1)    
    | elem (x,y) bottomRightCornerAtom && y<num+1 = goVerticalPath start num (x,y+1) 1 atomsCoord -- move downward to east direction (+1)
    
  

    | x>=num+1 && direction==1 = (start,Path (East,y))
    | x<=0 && direction==(-1)= (start,Path (West,y))
    | x>0 && direction==(-1) = goHorizontalPath start num (x-1,y) (-1) atomsCoord
    | x<num+1 && direction==1 = goHorizontalPath start num (x+1,y) 1 atomsCoord

    
    -- | (elem (x,y) topLeftCornerAtom || elem (x,y) bottomLeftCornerAtom)&& x==0 = (start,Reflect)
    -- | (elem (x,y) topRightCornerAtom || elem (x,y) bottomRightCornerAtom)&& x==num+1 = (start,Reflect)
    -- | x==num+1 && direction==1 = (start,Path(East,y))
    -- | x==0  && direction==(-1) = (start,Path(West,y))
    -- | (elem (x,y) topLeftCornerAtom ||elem (x,y) topRightCornerAtom)&& y>0 = goVerticalPath start num (x,y-1) (-1) atomsCoord-- move upward to north direction (-1)
    -- | (elem (x,y) bottomLeftCornerAtom ||elem (x,y) bottomRightCornerAtom) && y<num+1 = goVerticalPath start num (x,y+1) 1 atomsCoord -- move downward to south direction (+1)
    -- | (x,y) `elem` atomsCoord = (start,Absorb)
    -- | x>0 && direction==(-1) = goHorizontalPath start num (x-1,y) (-1) atomsCoord -- move left to west direction (-1)
    -- | x<num+1 && direction==1 = goHorizontalPath start num (x+1,y) 1 atomsCoord --move right to east direction (+1)
    | otherwise =goHorizontalPath start num (x+direction,y) direction atomsCoord
        where
            topLeftCornerAtom = detectTopLeftCoord atomsCoord
            topRightCornerAtom = detectTopRightCoord atomsCoord
            bottomLeftCornerAtom = detectBottomLeftCoord atomsCoord
            bottomRightCornerAtom = detectBottomRightCoord atomsCoord

-- return a list of the positions of exactly N atoms that gives rise to the given list of interactions
solveBB :: Int -> Interactions ->Atoms
solveBB numOfAtoms interactions = do
    let sizeOfMap = findMapSize interactions 0
    let results = detectPossibleAtoms numOfAtoms sizeOfMap interactions
    head results

-- return the max number of the map
findMapSize :: Interactions -> Int -> Int
findMapSize xs maxSize = foldl (\ maxSize x -> max maxSize (snd (fst x))) maxSize xs

findCoordInMap :: Int -> Atoms
findCoordInMap num = [(x,y) | x<-[1..num], y<-[1..num]]

detectPossibleAtoms :: Int -> Int -> Interactions -> [Atoms]
detectPossibleAtoms numOfAtoms mapSize interactions =
    [atoms | atoms<- possibleAtoms, 
    let testing = calcBBInteractions mapSize atoms,
    contains interactions testing, 
    not (null testing)]
    where
        allCoordInMap = findCoordInMap mapSize
        possibleAtoms = combinationsOf numOfAtoms allCoordInMap

-- generate all combinations of atoms
subsets :: Int-> Atoms -> [Atoms]
subsets 0 _ = [[]]
subsets _ [] = []
subsets n (x:xs) =map (x :) (subsets (n - 1) xs) ++ subsets n xs

combinationsOf :: Int -> [a] -> [[a]]
combinationsOf 1 as        = map pure as
combinationsOf k as@(x:xs) = run (l-1) (k-1) as $ combinationsOf (k-1) xs
                             where
                             l = length as

                             run :: Int -> Int -> [a] -> [[a]] -> [[a]]
                             run n k ys cs | n == k    = map (ys ++) cs
                                           | otherwise = map (q:) cs ++ run (n-1) k qs (drop dc cs)
                                           where
                                           (q:qs) = take (n-k+1) ys
                                           dc     = product [(n-k+1)..(n-1)] `div` product [1..(k-1)]

-- contains this element
contains :: Interactions -> Interactions -> Bool
contains list []= True
contains list susbset@(x:xs)= if x `elem` list then contains list xs else False
                                                
main :: IO ()
main = do
    putStrLn "Challenge1: Calculate all the interactions"
    print(calcBBInteractions 8 [(2,3),(7,3),(4,6),(7,8)])

    putStrLn "Challenge2: Solve Black Box Game"
    print(solveBB 2 (calcBBInteractions 8 [(2,3),(7,3)]))
    print(solveBB 3 (calcBBInteractions 8 [(2,3),(7,3),(4,6)]))
    print(solveBB 4 (calcBBInteractions 8 [(2,3),(3,3),(4,4),(1,2)]))
    print(detectPossibleAtoms 1 3 [((North,1),Path(West,2))])
    print(solveBB 4 (calcBBInteractions 8 [(2,3),(7,3),(4,6),(7,8)]))