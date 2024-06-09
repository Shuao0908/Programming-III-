import Foreign (Storable(sizeOf))
-- CW2 Challenge 1
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
            finalPathNorth = pathFromNorthCoord  [(x,y) | x<-[1..num] ,y<-[0]] atomsCoord num
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

goVerticalPath :: EdgePos -> Int -> Pos ->Int -> Atoms -> (EdgePos,Marking)
goVerticalPath start num (x,y) direction atomsCoord 
    | elem (x,y) topRightCornerAtom && direction==1 && y==0 = (start,Reflect)
    | elem (x,y) bottomRightCornerAtom && direction==(-1) && y==num+1 = (start,Reflect)
    | elem (x,y) topLeftCornerAtom && direction==1 && y==0 = (start,Reflect)
    | elem (x,y) bottomLeftCornerAtom && direction==(-1) && y==num+1 = (start,Reflect)

    | direction==(-1) && y<=0 = (start, Path (North,x)) -- reach the North Side
    | direction==1 && y>=num+1 = (start, Path (South,x)) -- reach the South Side

    | elem (x,y) atomsCoord =(start, Absorb)

    | elem (x,y) topRightCornerAtom = goHorizontalPath start num (x+1,y) 1 atomsCoord --go to west
    | elem (x,y) bottomRightCornerAtom = goHorizontalPath start num (x+1,y) 1 atomsCoord -- go to west
    | elem (x,y) topLeftCornerAtom = goHorizontalPath start num (x-1,y) (-1) atomsCoord -- go to east 
    | elem (x,y) bottomLeftCornerAtom = goHorizontalPath start num (x-1,y) (-1) atomsCoord -- go to east

    | otherwise = goVerticalPath start num (x,y+direction) direction atomsCoord -- based on the direction to go up or down
        where
            topLeftCornerAtom = detectTopLeftCoord atomsCoord
            topRightCornerAtom = detectTopRightCoord atomsCoord
            bottomLeftCornerAtom = detectBottomLeftCoord atomsCoord
            bottomRightCornerAtom = detectBottomRightCoord atomsCoord

goHorizontalPath :: EdgePos -> Int -> Pos ->Int -> Atoms -> (EdgePos,Marking)
goHorizontalPath start num (x,y) direction atomsCoord 
    | elem (x,y) topRightCornerAtom && direction==1 && x==0 = (start,Reflect)
    | elem (x,y) bottomRightCornerAtom && direction==1 && x==0 = (start,Reflect)
    | elem (x,y) topLeftCornerAtom && direction==(-1) && x==num+1 = (start,Reflect)
    | elem (x,y) bottomLeftCornerAtom && direction==(-1) && x==num+1 = (start,Reflect)
    
    | direction==1 && x>=num+1 = (start, Path (East,y))
    | direction==(-1) && x<=0 = (start, Path (West,y))

    | elem (x,y) atomsCoord =(start, Absorb)

    | elem (x,y) topRightCornerAtom = goVerticalPath start num (x,y-1) (-1) atomsCoord
    | elem (x,y) bottomRightCornerAtom = goVerticalPath start num (x,y+1) 1 atomsCoord
    | elem (x,y) topLeftCornerAtom = goVerticalPath start num (x,y-1) (-1) atomsCoord
    | elem (x,y) bottomLeftCornerAtom = goVerticalPath start num (x,y+1) 1 atomsCoord

    | otherwise = goHorizontalPath start num (x+direction,y) direction atomsCoord
        where
            topLeftCornerAtom = detectTopLeftCoord atomsCoord
            topRightCornerAtom = detectTopRightCoord atomsCoord
            bottomLeftCornerAtom = detectBottomLeftCoord atomsCoord
            bottomRightCornerAtom = detectBottomRightCoord atomsCoord

solveBB :: Int -> Interactions -> Atoms 
solveBB numOfAtoms interactions = filterAtoms (findMapSize interactions 0) numOfAtoms interactions

-- return the max number of the map
findMapSize :: Interactions -> Int -> Int
findMapSize xs maxSize = foldl (\ maxSize x -> max maxSize (snd (fst x))) maxSize xs

--same function as head
getFirstElement :: [Atoms] ->Atoms
getFirstElement [] = []
getFirstElement (x:xs) = x

-- to check element exists in this list or not
contains :: Interactions -> Interactions -> Bool
contains set [] = True
contains set subset@(x:xs) = if x `elem` set then contains set xs else False

--filter the atoms that doesn't give the same interactions
filterAtoms :: Int -> Int -> Interactions -> Atoms 
filterAtoms gridSize n interactions = getFirstElement 
  [atoms | atoms<-possibleAtoms, let testCase=(calcBBInteractions gridSize atoms),  contains interactions testCase, (length testCase)>0]
    where possibleAtoms = generateAtomCombinations n gridSize

--generate possible atom combinations based on a grid size
generateAtomCombinations::Int->Int->[Atoms]
generateAtomCombinations numOfAtoms gridSize = subsets numOfAtoms allCoordInMap
  where allCoordInMap = [(x, y) | x<-[1..gridSize], y<-[1..gridSize]]

-- generate all combinations of atoms
--for grid size 8 and wants 4 atoms it needs 635,376 combinations (8C3)
subsets :: Int-> Atoms -> [Atoms]
subsets 0 _ = [[]]
subsets _ [] = []
subsets n (x:xs) =map (x :) (subsets (n - 1) xs) ++ subsets n xs

main = do
  let atomsArr::Atoms = [(2,3),(7,3),(4,6),(7,8)] --1-based, where 0 is edge and gridSize+1 is edge
  let gridSize::Int = 8
  let challenge1 = calcBBInteractions gridSize atomsArr
  putStrLn("Challenge 1 CalcBBInteractions = "++show challenge1)

  let challenge2= solveBB 3 (calcBBInteractions 5 [(1,4),(1,3),(5,5)])
  putStrLn("Challenge 2 Solve Black Box= "++show challenge2)

  let challenge2b =filterAtoms 8 2 [((North,1),Path (West,2))]
  putStrLn("Challenge 2 Solve Black Box = "++show challenge2b)

  let challenge2c = solveBB 4 challenge1
  putStrLn("Challenge 2 Solve Black Box = "++show challenge2c)

  let challenge2d = solveBB 2 (calcBBInteractions 8 [(2,3),(7,3)]) 
  putStrLn("Challenge 2 Solve Black Box = "++show challenge2d)   

  let challenge2e = solveBB 3 (calcBBInteractions 8 [(2,3),(7,3),(4,6)])
  putStrLn("Challenge 2 Solve Black Box = "++show challenge2e)
