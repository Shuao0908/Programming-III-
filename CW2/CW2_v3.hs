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

goVerticalPath :: EdgePos -> Int -> Pos -> Int  -> Atoms -> (EdgePos,Marking)
goVerticalPath start num (x,y) direction atomsCoord
    | (x,y) `elem` atomsCoord = (start,Absorb)

    | elem (x,y) topLeftCornerAtom && x>0 = goHorizontalPath start num (x-1,y) (-1) atomsCoord-- move left to west direction (-1)
    | elem (x,y) topLeftCornerAtom && y==0 = (start,Reflect)

    | elem (x,y) topRightCornerAtom && x<(num+1) = goHorizontalPath start num (x+1,y) 1 atomsCoord -- move right to east direction (+1)
    | elem (x,y) topRightCornerAtom && y==0 = (start,Reflect)

    | elem (x,y) bottomLeftCornerAtom && y==num+1 = (start,Reflect)
    | elem (x,y) bottomLeftCornerAtom && x>0 = goHorizontalPath start num (x-1,y) (-1) atomsCoord -- move left to west direction (-1)

    | elem (x,y) bottomRightCornerAtom && y==num+1 = (start,Reflect)
    | elem (x,y) bottomRightCornerAtom && x<num+1 = goHorizontalPath start num (x+1,y) 1 atomsCoord -- move right to east direction (+1)

    | y>0 && direction==(-1) = goVerticalPath start num (x,y-1) (-1) atomsCoord
    | y<(num+1) && direction==1 = goVerticalPath start num (x,y+1) 1  atomsCoord

    | y==(num+1) = (start,Path (South,x))
    | y==0 = (start,Path (North,x))
        where
            topLeftCornerAtom = detectTopLeftCoord atomsCoord
            topRightCornerAtom = detectTopRightCoord atomsCoord
            bottomLeftCornerAtom = detectBottomLeftCoord atomsCoord
            bottomRightCornerAtom = detectBottomRightCoord atomsCoord

goHorizontalPath:: EdgePos -> Int -> Pos -> Int -> Atoms -> (EdgePos,Marking)
goHorizontalPath start num (x,y) direction atomsCoord
    | (x,y) `elem` atomsCoord = (start,Absorb)

    | elem (x,y) topLeftCornerAtom && y>0 = goVerticalPath start num (x,y-1) (-1) atomsCoord-- move upward to north direction (-1)
    | elem (x,y) topLeftCornerAtom && x==0 = (start,Reflect)

    | elem (x,y) topRightCornerAtom && y<num+1 = goVerticalPath start num (x,y-1) (-1) atomsCoord -- move upward to north direction (-1)
    | elem (x,y) topRightCornerAtom && x==num+1 = (start,Reflect)

    | elem (x,y) bottomLeftCornerAtom && y>0 = goVerticalPath start num (x,y+1) 1 atomsCoord -- move downward to west direction (+1)
    | elem (x,y) bottomLeftCornerAtom && x==0 = (start,Reflect)

    | elem (x,y) bottomRightCornerAtom && y<num+1 = goVerticalPath start num (x,y+1) 1 atomsCoord -- move downward to east direction (+1)
    | elem (x,y) bottomRightCornerAtom && x==num+1 = (start,Reflect)

    | x>0 && direction==(-1) = goHorizontalPath start num (x-1,y) (-1) atomsCoord
    | x<num+1 && direction==1 = goHorizontalPath start num (x+1,y) 1 atomsCoord

    | x==num+1 = (start,Path (East,y))
    | x==0 = (start,Path (West,y))
        where
            topLeftCornerAtom = detectTopLeftCoord atomsCoord
            topRightCornerAtom = detectTopRightCoord atomsCoord
            bottomLeftCornerAtom = detectBottomLeftCoord atomsCoord
            bottomRightCornerAtom = detectBottomRightCoord atomsCoord

-- -- return a list of the positions of exactly N atoms that gives rise to the given list of interactions
-- solveBB :: Int -> Interactions ->IO()
-- solveBB numOfAtoms interactions = do
--     let sizeOfMap = findMapSize interactions 0
--     print sizeOfMap
--     let results = detectPossibleAtoms numOfAtoms sizeOfMap interactions
--     print results

-- -- return the max number of the map
-- findMapSize :: Interactions -> Int -> Int
-- findMapSize xs maxSize
--   = foldl (\ maxSize x -> max maxSize (snd (fst x))) maxSize xs

-- findCoordInMap :: Int -> Atoms
-- findCoordInMap num = [(x,y) | x<-[1..num], y<-[1..num]]

-- detectPossibleAtoms :: Int -> Int -> Interactions -> Atoms
-- detectPossibleAtoms numOfAtoms gridSize interactions = head [atoms | atoms<-possibleAtoms, contains interactions (calcBBInteractions gridSize atoms), length (calcBBInteractions gridSize atoms)>0]
--     where
--         allCoordInMap = findCoordInMap gridSize
--         possibleAtoms = subsets numOfAtoms allCoordInMap


-- subsets :: Int-> Atoms -> [Atoms]
-- subsets 0 _ = [[]]
-- subsets _ [] = []
-- subsets n (x:xs) =map (x :) (subsets (n - 1) xs) ++ subsets n xs

-- -- contains this element
-- contains :: Interactions -> Interactions -> Bool
-- contains [_] _ = True
-- contains [] _ = False
-- contains list (x:xs) = (x `elem` list) && contains list xs

-- main :: IO ()
-- main = do
--     print (calcBBInteractions 8 [(2,3),(7,3),(4,6),(7,8)])
--     (solveBB 4 (calcBBInteractions 8 [(2,3),(7,3),(4,6),(7,8)]))
getGridSize::Interactions->Int->Int
getGridSize [] curmax = curmax
getGridSize (((side,num),mark):xs) curmax = getGridSize xs (maximum [num,curmax])

solveBB :: Int -> Interactions -> Atoms 
solveBB n interactions = solveBB_grid (getGridSize interactions 0) n interactions

safeHead [] = []
safeHead (x:xs) = x

--to support partial interaction, need this "contains" instead of just checking whether the interaction "equals"
contains set [] = True
contains set subset@(x:xs)
  | x `elem` set = contains set xs
  | otherwise = False

--solveBB_grid is the same but with fixed grid size
solveBB_grid :: Int -> Int -> Interactions -> Atoms 
--we take the result atoms if it matches with the interactions that is specified, and if the length>0
solveBB_grid size n interactions = head
  [atoms | atoms<-possibleAtoms, let result=(calcBBInteractions size atoms),  contains interactions result, (length result)>0]
    where possibleAtoms = atomCombinations n size

--create possible combinations of n elements
--https://stackoverflow.com/questions/52602474/function-to-generate-the-unique-combinations-of-a-list-in-haskell
--there are several variations there, but this one gives the best performance:
--I initially tested my own code but it generates the 635,376 combinations very slowly, so i changed it to that one
subsequencesOfSize :: Int -> [a] -> [[a]]
subsequencesOfSize n xs = let l = length xs
                          in if (n > l) then []
                             else subsequencesBySize xs !! (l-n)
 where
   subsequencesBySize [] = [[[]]]
   subsequencesBySize (x:xs) = let next = subsequencesBySize xs
                               in zipWith (++)
                                    ([]:next)
                                    ( map (map (x:)) next ++ [[]] )

--generate possible atom combinations based on a grid size
--k is how many elements
atomCombinations::Int->Int->[Atoms]
atomCombinations k gridSize = subsequencesOfSize k allAtoms
  where allAtoms = [(x, y) | x<-[1..gridSize], y<-[1..gridSize]]

main = do
  let atomsArr::Atoms = [(2,3),(7,3),(4,6),(7,8)] --1-based, where 0 is edge and gridSize+1 is edge
  let gridSize::Int = 8
  let resQ1 = calcBBInteractions gridSize atomsArr
  putStrLn("Q1 = "++show resQ1)

  let resQ2a = solveBB 3 (calcBBInteractions 5 [(1,4),(1,3),(5,5)])
  putStrLn("Q2 with complete interactions (grid size 5) = "++show resQ2a)

  let resQ2c = solveBB_grid 8 2 [((North,1),Path (West,2))]
  putStrLn("Q2 with partial interactions = "++show resQ2c)

  let resQ2b = solveBB 4 resQ1
  putStrLn("Q2 with complete interactions (grid size 8) = "++show resQ2b)

  print(solveBB 2 (calcBBInteractions 8 [(2,3),(7,3)]))    
  print(solveBB 3 (calcBBInteractions 8 [(2,3),(7,3),(4,6)]))
  print(solveBB 4 (calcBBInteractions 5 [(2,3),(3,3),(4,4),(1,2)]))
  print(solveBB 4 (calcBBInteractions 8 [(2,3),(7,3),(4,6),(7,8)]))