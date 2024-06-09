{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
type Pos = (Int,Int)
data Side = North | East | South | West deriving (Show,Eq,Ord)
type EdgePos = (Side, Int)
type Atoms = [Pos]
type Interactions = [ (EdgePos , Marking) ]
data Marking = Absorb | Reflect | Path EdgePos deriving (Show, Eq)

goUpDown::EdgePos->Pos->Int->Int->Atoms->(EdgePos,Marking)
goUpDown sourcePos (x,y) dir size atoms --dir is -1 or +1
  | (hitRight || hitLeft) && justCame = (sourcePos, Reflect)
  | dir==(-1) && y<=0 = (sourcePos, Path (North,x))
  | dir==1 && y>=size+1 = (sourcePos, Path (South,x))
  | (x,y) `elem` atoms = (sourcePos, Absorb)
  | hitRight = goLeftRight sourcePos (x-1,y) (-1) size atoms --continue left
  | hitLeft = goLeftRight sourcePos (x+1,y) 1 size atoms --continue right
  | otherwise = goUpDown sourcePos (x,y+dir) dir size atoms
  where 
    hitRight = (x+1,y+dir) `elem` atoms --if go down, dir is +1 so check the atoms on the bottom side (vice versa for up)
    hitLeft = (x-1,y+dir) `elem` atoms
    justCame = (dir==(-1) && (y==size+1)) || (dir==1 && (y==0))

goLeftRight::EdgePos->Pos->Int->Int->Atoms->(EdgePos,Marking)
goLeftRight sourcePos (x,y) dir size atoms --dir is -1 or +1
  | (hitDown || hitUp) && justCame = (sourcePos, Reflect)
  | dir==(-1) && x<=0 = (sourcePos, Path (West,y))
  | dir==1 && x>=size+1 = (sourcePos, Path (East,y))
  | (x,y) `elem` atoms = (sourcePos, Absorb)
  | hitDown = goUpDown sourcePos (x,y-1) (-1) size atoms --continue up
  | hitUp = goUpDown sourcePos (x,y+1) 1 size atoms --continue down
  | otherwise = goLeftRight sourcePos (x+dir,y) dir size atoms
  where 
    hitDown = (x+dir,y+1) `elem` atoms --if go left, dir is -1 so check the atoms on the left side (vice versa for right)
    hitUp = (x+dir,y-1) `elem` atoms
    justCame = (dir==(-1) && (x==size+1)) || (dir==1 && (x==0))

calcBBInteractions :: Int -> Atoms -> Interactions
--start from each position (north, south, west, east) and combine all the results
calcBBInteractions size atoms = 
  [(goUpDown (North,i) (i,0) 1 size atoms) | i<-[1..size]]++
  [(goLeftRight (East,i) (size+1,i) (-1) size atoms) | i<-[1..size]]++
  [(goUpDown (South,i) (i,size+1) (-1) size atoms) | i<-[1..size]]++
  [(goLeftRight (West,i) (0,i) 1 size atoms) | i<-[1..size]]
  

--based on interactions list, get the grid size
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
solveBB_grid size n interactions = safeHead 
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