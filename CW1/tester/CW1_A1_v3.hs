import Data.Char

-- Function to encrypt a single character
-- 'A' is 0, 'B' is 1, ..., 'Z' is 25
shift :: Char -> Char -> Char
shift keyChar inputChar
    | isUpper inputChar = chr $ ord 'A' + (ord inputChar - ord 'A' + ord keyChar - ord 'A') `mod` 26
    | otherwise = inputChar

-- Function to encrypt a message using the Vigenère cipher
vigenereEncrypt :: String -> String -> String
vigenereEncrypt keyword message = zipWith shift (cycle keyword) (map toUpper message)

-- Function to decrypt a message
vigenereDecrypt :: String -> String -> String
vigenereDecrypt keyword ciphertext = zipWith shift (cycle (map negateKeyword keyword)) ciphertext
  where
    negateKeyword c = chr $ (26 - (ord c - ord 'A')) `mod` 26 + ord 'A'

-- Function to create a pair of Vigenère encryption and decryption functions
vigenere :: String -> (String -> String, String -> String)
vigenere keyword = (vigenereEncrypt keyword, vigenereDecrypt keyword)

main :: IO ()
main = do
    putStrLn "Enter the keyword: "
    keyword <- getLine

    let (encrypt, decrypt) = vigenere keyword

    putStrLn "Enter the message to encrypt: "
    message <- getLine

    let encryptedMessage = encrypt message
    putStrLn "Encrypted message: "
    putStrLn encryptedMessage

    let decryptedMessage = decrypt encryptedMessage
    putStrLn "Decrypted message: "
    putStrLn decryptedMessage
