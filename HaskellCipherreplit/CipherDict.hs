module CipherDict (encrypt1, decrypt1, karakters, nrandom, index, get) where

-- Characters
capitals :: [Char]
capitals = ['A'..'Z'] 
smalls :: [Char]
smalls = ['a'..'z'] 
alphabets :: [Char]
alphabets = capitals ++ smalls 
digits :: [Char]
digits = ['0'..'9'] 
others :: [Char]
others = ['!'..'/'] ++ [':'..'@'] ++ ['['..'`']  ++  ['{'..'~']  
space :: [Char]
space = [' '] 
karakters :: [Char]  -- list of all characters that would be used in a standard english message in a "normal" order
karakters = alphabets ++ others ++ space ++ digits 

-- Making of the "randomized" lists:
index :: Eq a => [a] -> a  -> Int  -- takes a list and an element and returns the index of that element on that list (if it is in there)
index l x = length $ takeWhile (/= x)  l

get :: Eq a => [a] -> Int  ->  a  -- given a list and an int finds the element in the list with that index (cyclic /modular)
get l n = if (n>0) then cycle l !! n else get l (n + length l)  

psychle :: Eq a => [a] -> Int -> [a]  -- "shuffles" a list by turning it into indices, multiplying the elements by some int m and getting back what those elements would be in the original indexing. 
psychle l m = map (get l ) ( map (*m) (map (index l ) l ) ) --note that if the integer the indices are multiplied by is not coprime with the length of the list then different elements could get mapped to the same thing (no longer invertible)

swap :: Eq a => [a] -> Int -> Int -> [a]  -- swaps the nth and mth elements of the list. 
swap l n m = 
 if (n<m) then 
  fst(splitAt n l) ++ (l !! m) :  tail ( fst(splitAt (m-n) (snd(splitAt n l)) ) ) ++ (l !! n) : tail (snd(splitAt (m-n) (snd(splitAt n l)) ) )
 else swap l m n
 
-- Arbitrary shufflings:
a1 :: [Char]
a2 :: [Char]
a3 :: [Char]
a4 :: [Char]
a1 = swap alphabets 0 37 
a2 = psychle a1 41 
a3 = swap a2 13 50 
a4 = psychle (swap a3 0 31) 11 
arandom :: [Char]
arandom = a4 
o1 :: [Char]
o2 :: [Char]
o3 :: [Char]
o4 :: [Char]
o1 = swap (others ++ digits) 0 25 
o2 = psychle o1 19 
o3 = swap o2 1 13 
o4 = psychle o3 17 
orandom :: [Char]
orandom = o4 
newcharacters :: [Char]
newcharacters = orandom ++ arandom  -- length 94 = 2*47
n1 :: [Char]
n2 :: [Char]
n3 :: [Char]
n4 :: [Char]
n1 = swap newcharacters 0 77 
n2 = psychle n1 43 
n3 = swap n2 53 81 
n4 = psychle n3 63 
nrandom' :: [Char]
nrandom' = n4 
n1' :: [Char]
n2' :: [Char]
n3' :: [Char]
n4' :: [Char]
n1' = swap ( ' ' : nrandom' ) 0 51  -- length 95 = 5*19
n2' = psychle n1' 36 
n3' = swap n2' 4 44 
n4' = psychle n3' 83 
nrandom :: [Char]
nrandom = n4' 

encrypt1 :: [Char] -> [Char]
decrypt1 :: [Char] -> [Char]
encrypt1 a = map (get nrandom ) (map (index karakters) a)
decrypt1 a = map (get karakters ) (map (index nrandom) a)