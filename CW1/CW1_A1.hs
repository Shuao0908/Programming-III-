import Data.Char (toUpper, isAlpha)
-- CW1 A1
vigenere :: String -> (String -> String, String -> String)
vigenere key = (vigEncrypt key, vigDecrypt key)

vigEncrypt :: String -> String -> String
vigEncrypt key message = zipWith find (cycle key) (convert message)

vigDecrypt :: String -> String -> String
vigDecrypt key = zipWith find (cycle (map negateKeyword key)) 
    where 
        negateKeyword c = toEnum ((26 - (fromEnum c - fromEnum 'A'))`mod` 26 + fromEnum 'A')

-- shiftCharPosition 
-- create the list for the character and position eg: A-0, B-1, Z-25
-- find the postion of the charcter
find :: Char -> Char ->Char
find char key= toEnum (fromEnum 'A' + finalPosition)
    where
        charPosition = fromEnum char - fromEnum 'A'
        keyPostion = fromEnum key - fromEnum 'A'
        finalPosition = (charPosition + keyPostion) `mod` 26

-- remove all non-alphabetic character and whitespace
-- change all character to Uppercase then filter 
convert :: String -> String
convert = map toUpper . filter isAlpha

main :: IO()
main = do
    -- input case by tester
    putStr "Enter the keyword you want to use: "
    keyword <- getLine

    let (encryptMsg,decryptMsg)= vigenere (convert keyword) 

    putStr "Enter the message you want to encrypt: "
    message <- getLine 

    let encryptedMessage = encryptMsg (convert message)
    putStrLn $ "Encrypted message: "++ encryptedMessage
    putStrLn $ "Decrypted message: "++  decryptMsg encryptedMessage

    -- Test Case 1
    let keyword1 = "lemon"
    let message1 = "attackdawn"
    let (encryptMsg, decryptMsg) =vigenere (convert keyword1)
    putStrLn $ "\nEnter the keyword you want to use: " ++ keyword1 --lemon
    putStrLn $ "Enter the message you want to encrypt: " ++ message1 --attackdawn

    let encryptedMessage = encryptMsg (convert message1)
    putStrLn $ "Encrypted message: "++ encryptedMessage --Output: LXFOPVHMKA
    putStrLn $ "Decrypted message: "++  decryptMsg encryptedMessage --Output: ATTACKDAWN

    -- Test Case 2
    let keyword2 = "keyword"
    let message2 = "he llo"
    let (encryptMsg, decryptMsg) =vigenere (convert keyword2)
    putStrLn $ "\nEnter the keyword you want to use: " ++ keyword2 --keyword
    putStrLn $ "Enter the message you want to encrypt: " ++ message2 --he llo

    let encryptedMessage = encryptMsg (convert message2)
    putStrLn $ "Encrypted message: "++ encryptedMessage --Output: RIJHC
    putStrLn $ "Decrypted message: "++  decryptMsg encryptedMessage --Output: HELLO


