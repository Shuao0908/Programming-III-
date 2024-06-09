import Data.Char

-- Function to shift a character by a given offset (with wraparound)
shiftChar :: Int -> Char -> Char
shiftChar offset char
    | isUpper char = chr $ ord 'A' + (ord char - ord 'A' + offset) `mod` 26
    | isLower char = chr $ ord 'a' + (ord char - ord 'a' + offset) `mod` 26
    | otherwise = char

-- Function to encrypt a string using the Vigenère cipher with a given key
encryptVigenere :: String -> String -> String
encryptVigenere key plaintext = zipWith shiftChar offsets plaintext
  where
    offsets = map (subtract (ord 'A') . ord) (cycle key)

-- Function to decrypt a string using the Vigenère cipher with a given key
decryptVigenere :: String -> String -> String
decryptVigenere key ciphertext = zipWith (flip shiftChar) offsets ciphertext
  where
    offsets = map (subtract (ord 'A') . ord) (cycle key)

-- Usage example
main :: IO ()
main = do
    putStrLn "Enter the Vigenère key: "
    key <- getLine
    putStrLn "Enter the plaintext: "
    plaintext <- getLine

    let ciphertext = encryptVigenere key plaintext
    putStrLn "Encrypted text: "
    putStrLn ciphertext

    let decryptedText = decryptVigenere key ciphertext
    putStrLn "Decrypted text: "
    putStrLn decryptedText
