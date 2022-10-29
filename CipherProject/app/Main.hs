module Main (main, encrypt, decrypt) where

import CipherDict
--------------------------------------------------
import System.Random

-- Obscuring message using a password string
addstrings :: [Char] -> (Char, Char) -> Char
addstrings l (a, b) = get l ((index l a) + (index l b))
difstrings :: [Char] -> (Char, Char) -> Char
difstrings l (a, b) = get l ((index l a) - (index l b))

subcrypt2 :: [Char] -> [Char] -> [Char]  
desubcrypt2 :: [Char] -> [Char] -> [Char]
subcrypt2 p a = map (addstrings karakters)(zip a (cycle p))  
desubcrypt2 p a = map (difstrings karakters)(zip a (cycle p))

encrypt2 :: [Char] -> [Char] -> [Char]
decrypt2 :: [Char] -> [Char] -> [Char]
encrypt2 p a = encrypt1 (subcrypt2 p a)
decrypt2 p a = desubcrypt2 p (decrypt1 a)

-- Interacting with user
encrypt :: IO()
decrypt :: IO()

encrypt = do 
 putStrLn "Enter the message message you want to encrypt: "
 message <- getLine
 putStrLn "Enter the password (used to unravel the encrypted message): "
 pass <- getLine
 g <- return (mkStdGen (product (map (index nrandom) (message ++ pass) ) ) )
 k <- return (take 5 ( randomRs ('A', 'z') g ))
 putStrLn $ "The encrypted message is: " ++ (encrypt2 pass (encrypt2 k message) ) ++ " and your security key is: " ++ show(k) ++ " (remember this)"

decrypt = do
 putStrLn "Enter the message message you want to decrypt: "
 message <- getLine
 putStrLn "Enter the password: "
 pass <- getLine
 putStrLn "Enter the security key: "
 k <- getLine
 putStrLn $ "The message is: " ++ (decrypt2 k (decrypt2 pass message) )


main :: IO ()
main = do 
 encrypt
 print $ "Now let's check if it decrypts appropriately" 
 decrypt
