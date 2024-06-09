import Data.Char
vigenereCipher :: String -> String -> String
vigenereCipher keyword = zipWith (flip (shiftWith keyword)) keyword
    where
        shiftWith :: String -> Char -> Char -> Char
        shiftWith keyword char keywordChar =
            let base = ord 'a'
                lower = (ord char) - base
                keyLetter = ord keywordChar - base
                result = lower + keyLetter
            in
                if (ord char) < (ord 'a')
                then chr result
                else chr $ mod (result - 26) 26 + base

main :: IO ()
main = do
    putStrLn "Enter the plain text:"
    plainText <- getLine
    putStrLn "Enter the keyword:"
    keyword <- getLine
    let cipherText = vigenereCipher keyword plainText
    putStrLn $ "Cipher text: " ++ cipherText