module Main (main) where

import Lib
--------------------------------------------------
import System.Random

capitals = ['A'..'Z']
smalls = ['a'..'z']
alphabets = capitals ++ smalls
digits = ['0'..'9']
others = ['!'..'/'] ++ [':'..'@'] ++ ['['..'`']  ++  ['{'..'~'] -- 0 to 14, 15 to 21, 22 to 27, 28 to 31
space = [' ']
karakters = alphabets ++ others ++ [' '] ++ digits

-- Making of the "randomized" lists:
index :: Eq a => [a] -> a  -> Int
index l x = length $ takeWhile (/= x)  l

get :: Eq a => [a] -> Int  ->  a 
get l n = if (n>0) then cycle l !! n else get l (n + length l)  -- I frogging love this language

psychle :: Eq a => [a] -> Int -> [a]  -- usual GT *m of a cyclic group
psychle l m = map (get l ) ( map (*m) (map (index l ) l ) )

swap :: Eq a => [a] -> Int -> Int -> [a]  -- usual group theory 2-cycle
swap l n m = 
 if (n<m) then 
  fst(splitAt n l) ++ (l !! m) :  tail ( fst(splitAt (m-n) (snd(splitAt n l)) ) ) ++ (l !! n) : tail (snd(splitAt (m-n) (snd(splitAt n l)) ) )
 else swap l m n

nrandom = "Q+l$]Yf`Cg9?KR\\o;5pw8Ta-1bi4}|)GW[kr!%sz(Nd@Xe3BI6<J ~nu2.v'SZ*FDh#EL&_M>jqx\"Uy:Vc=7^0,HO/AP{mt" -- was nrandom'

encrypt1 :: [Char] -> [Char]
decrypt1 :: [Char] -> [Char]
encrypt1 a = map (get nrandom ) (map (index karakters) a)
decrypt1 a = map (get karakters ) (map (index nrandom) a)

addstrings :: [Char] -> (Char, Char) -> Char
addstrings l (a, b) = get l ((index l a) + (index l b))
difstrings :: [Char] -> (Char, Char) -> Char
difstrings l (a, b) = get l ((index l a) - (index l b))

subcrypt2 :: [Char] -> [Char] -> [Char]  -- takes pass and message and basically applies distrot to it. 
desubcrypt2 :: [Char] -> [Char] -> [Char]
subcrypt2 p a = map (addstrings karakters)(zip a (cycle p))  -- finite list of 2 tuples with fst being from message and snd being from pass cycle. => turn into list of numbers by mapping
desubcrypt2 p a = map (difstrings karakters)(zip a (cycle p))  -- oh boy, negative indexes... need fix. 

encrypt2 :: [Char] -> [Char] -> [Char]
decrypt2 :: [Char] -> [Char] -> [Char]
encrypt2 p a = encrypt1 (subcrypt2 p a)
decrypt2 p a = desubcrypt2 p (decrypt1 a)

encrypt3 = do 
 putStrLn "Enter the message message you want to encrypt: "
 message <- getLine
 putStrLn "Enter the password (used to unravel the encrypted message): "
 pass' <- getLine
 putStrLn $ "The encrypted message is: " ++ (encrypt2 pass' message) 

decrypt3 = do
 putStrLn "Enter the message message you want to decrypt: "
 message <- getLine
 putStrLn "Enter the password: "
 pass' <- getLine
 putStrLn $ "The message is: " ++ (decrypt2 pass' message) 


main :: IO ()
main = someFunc
