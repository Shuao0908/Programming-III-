import Data.List 
import Data.Foldable
import Data.Char
import Data.Function (on)
import System.Posix.Internals (lstat)
--CW1 A2

-- remove all non-alphabetic character and whitespace
-- change all character to Uppercase then filter 
convert :: String -> String
convert = map toUpper . filter isAlpha

-- sort the element by the second element with snd function
-- orderList = sortBy (compare `on` snd) 
-- descList = quicksortDesc  orderList 
frequency :: Int -> String -> [[(Char,Int)]]
frequency n ct = map (list . sort)  groupedCharacters where groupedCharacters = groupByPosition n ct

-- count the occurences of the character in the message
countCharacter:: Char -> String -> Int
countCharacter c = length . filter (==c)        

-- combine the character appears and its occurences into a list of tuples
-- and only remain the first element of the list of the same character
-- list = [(c,count) | c<- ['A'..'Z'], let count=countCharacter c ct ,count>0 | c <- nub ct]
list :: [Char] ->[(Char,Int)]
list str = [(c,countCharacter c str) | c <- nub str]

-- arrange them in descending order based on their frequency
quicksortDesc :: [(Char, Int)] -> [(Char, Int)]
quicksortDesc [] = []
quicksortDesc ((z,y):zs) = quicksortDesc rs ++ [(z,y)] ++ quicksortDesc ls
    where
        rs =[(a,b) | (a,b)<-zs, b>y]
        ls =[(a,b) | (a,b)<-zs, b<=y]

-- Split a list into n parts
-- split :: Int -> [(Char,Int)] -> [[(Char,Int)]]
-- split _ [] = [[],[]]
-- split n (x:xs)= let [(odds), (evens)] = split xs in [(x:evens), (odds)]
groupByPosition :: Int -> String -> [[Char]]
groupByPosition n ct = [ [ct !! i | i <- [pos, pos + n..length ct - 1]] | pos <- [0..n-1]]

main ::IO()
main = do
    -- user input case
    putStrLn "Enter the message you want to encrypt: "
    message <- getLine
    let keyword = "KEY" 
    print (frequency (length keyword) (convert message))

    -- test case 1 
    let message1 = "Hello World"
    putStrLn $ "Message to encrypt: " ++ message1
    print (frequency (length keyword) (convert message1))
    --[[('D',1),('H',1),('L',1),('O',1)],
    -- [('E',1),('O',1),('R',1)],
    -- [('L',2),('W',1)]]

    -- test case 2
    let message2 = "What do you want?"
    putStrLn $ "Message to encrypt: " ++ message2
    print (frequency (length keyword) (convert message2))
    -- [[('T',2),('W',2),('Y',1)],
    --  [('A',1),('D',1),('H',1),('O',1)],
    --  [('A',1),('N',1),('O',1),('U',1)]]