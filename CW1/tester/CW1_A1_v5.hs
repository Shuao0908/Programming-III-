module CW1_A1_v5 where
import Data.Char (toUpper, isAlpha)

-- Function to convert a character to uppercase
toUpperChar :: Char -> Char
toUpperChar = toUpper

-- Function to shift a character by a given offset
shiftChar :: Int -> Char -> Char
shiftChar offset c
  | isAlpha c = toEnum $ (fromEnum c - fromEnum 'A' + offset) `mod` 26 + fromEnum 'A'
  | otherwise = c

-- Function to encrypt a character using the Vigenere cipher
encryptChar :: Char -> Char -> Char
encryptChar keyChar messageChar =
  let offset = fromEnum keyChar - fromEnum 'A'
  in shiftChar offset messageChar

-- Function to decrypt a character using the Vigenere cipher
decryptChar :: Char -> Char -> Char
decryptChar keyChar messageChar =
  let offset = (26 - (fromEnum keyChar - fromEnum 'A')) `mod` 26
  in shiftChar offset messageChar

-- Function to perform Vigenere encryption
vigEncrypt :: String -> String -> String
vigEncrypt keyword message = zipWith encryptChar (cycle keyword) message

-- Function to perform Vigenere decryption
vigDecrypt :: String -> String -> String
vigDecrypt keyword message = zipWith decryptChar (cycle keyword) message

-- Function to remove non-alphabetic characters and convert to uppercase
convert :: String -> String
convert = map toUpperChar . filter isAlpha

main :: IO ()
main = do
    putStr "Enter the keyword you want to use: "
    keyword <- getLine

    putStr "Enter the message you want to encrypt: "
    message <- getLine

    let encryptedMessage = vigEncrypt (convert keyword) (convert message)
    putStrLn $ "Encrypted message: " ++ encryptedMessage

    let decryptedMessage = vigDecrypt (convert keyword) encryptedMessage
    putStrLn $ "Decrypted message: " ++ decryptedMessage
