import Data.Char
-- CW1 A5
hextonum :: Char -> Int
hextonum x | x `elem` ['0'..'9'] = fromEnum x - fromEnum '0'
           | x `elem` ['a'..'f'] = 10 + (fromEnum x - fromEnum 'a')
           | x `elem` ['A'..'F'] = 10 + (fromEnum x - fromEnum 'A')

numtohex :: Int -> Char
numtohex x | x `elem` [0..9] = toEnum(fromEnum x - fromEnum '0')
           | x `elem` [10..15] = toEnum (fromEnum 'a' +(x-10))
           

main :: IO()
main = do
    putStrLn $ "Change a from hex to num:"++ show (hextonum 'a') --10
    putStrLn $ "Change 15 from num to hex:"++ [numtohex 15] --'f'
    
    let hexChars = "0123456789abcdefABCDEF"
    let decimalNumbers = [0..15]
    let hexToDecimal = map hextonum "0123456789abcdefABCDEF"
    let decimalToHex = map numtohex decimalNumbers 

    putStrLn "Hex to Decimal Conversion:"
    putStrLn hexChars
    print hexToDecimal
    
    putStrLn "\nDecimal to Hex Conversion:"
    print decimalNumbers
    putStrLn decimalToHex

    putStr "The test result: "
    print ( map (hextonum . numtohex) decimalNumbers)
    