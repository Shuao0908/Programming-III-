import Data.Char

hextonum :: Char -> Int
hextonum c
    | '0' <= c && c <= '9' = ord c - ord '0'
    | 'a' <= c && c <= 'f' = ord c - ord 'a' + 10
    | 'A' <= c && c <= 'F' = ord c - ord 'A' + 10
    | otherwise = error "Invalid hexadecimal character"

numtohex :: Int -> Char
numtohex n
    | 0 <= n && n <= 9 = chr (ord '0' + n)
    | 10 <= n && n <= 15 = chr (ord 'A' + n - 10)
    | otherwise = error "Invalid decimal number"

-- Test your functions
main :: IO ()
main = do
    let hexChars = "0123456789abcdefABCDEF"
    let decimalNumbers = [0..15]
    let hexToDecimal = map hextonum hexChars
    let decimalToHex = map numtohex decimalNumbers

    putStrLn "Hex to Decimal Conversion:"
    putStrLn hexChars
    print hexToDecimal

    putStrLn "\nDecimal to Hex Conversion:"
    print decimalNumbers
    putStrLn decimalToHex

    let roundtripTest = map (hextonum . numtohex) decimalNumbers
    putStrLn "\nRoundtrip Test:"
    print roundtripTest

