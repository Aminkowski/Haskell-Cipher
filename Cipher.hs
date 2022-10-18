--Cypher :: [Char] -> [Char]
--Cypher 

-- Cipher #1: I want it to take a string, and... 
--  just associate every character with another unique 
--  character (one-to-one so that it is invertible). 

capitals = ['A'..'Z']
smalls = ['a'..'z']
alphabets = capitals ++ smalls
digits = ['0'..'9']
others = ['!'..'/'] ++ [':'..'@'] ++ ['['..'`']  ++  ['{'..'~'] -- 0 to 14, 15 to 21, 22 to 27, 28 to 31
space = [' ']
karakters = alphabets ++ others ++ [' '] ++ digits

-- [nul..ful] returns \NUL\SOH\STX\ETX\EOT\ENQ\ACK\a\b\t\n\v\f\r\SO\SI\DLE\DC1\DC2\DC3\DC4\NAK\SYN\ETB\CAN\EM\SUB\ESC\FS\GS\RS\US !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~\DEL\128\129\130\131\132\133\134\135\136\137\138\139\140\141 ... 
-- assuming it continues to have \number elements from then on, the only characters I can see someone typing in an (english) message are the following: !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~
{- Categorising them we have: 
The alphabets: ABCDEFGHIJKLMNOPQRSTUVWXYZ  and  abcdefghijklmnopqrstuvwxyz 
The numbers: 0123456789
and the others: ! \" #$%&'()*+,-./  and  :;<=>?@  and  [\\]^_`  and  {|}~
-}
-- the alphabets each have length 26, the numbers 10, and the others I think 32. also add in the ' ' characters so +1. 
-- I want to make a "random" list of a mixture of all the characters and then use map to turn regular characters into their counterpart that happens to be in the same spot in this new random rearrangement. 
-- now how do I make a random mixture...? ideally I'd like the new random list to be affected by the password. 
-- I think I'll have that be in the advanced versions. For now lemme make a n arbitrary random list. 
-- the alphabet characters are used most commonly, and since there are 52+1 of them as opposed to 
-- the 42 "other" characters, I want to have 42 of the alphabets go to characters. 
-- do I keep space go to space or not? on one hand it would look nicer and it is extremely easy to figure out, 
-- on the other hand for later functions it's less safe? 
-- also I was thinking of using succ and pred earlier but I probably won't do that since unless I can define my own type it's going to go out of bounds. 

{-
so the cardinality of digits ++ others is 42, which is a bit annoying since it could have been 43
this means that ...
ideally I'd like to do this with succ and pred, but since that either leads to bugs or 
annoying complications I'll just work with the lists. 
What I'll do is I'll take a character and redirect it to another character cyclically. 
Since I don't know how to just succ or pred on the elements of the list, I'll just
take the index and do math on that. 
-}
{-
--Get Index
aindex :: Char -> Int
oindex :: Char -> Int
aindex x = length $ takeWhile (/= x)  alphabets
oindex x = length $ takeWhile (/= x)  (others ++ digits)

--Inverses 
aget :: Int -> Char
oget :: Int -> Char
aget n = cycle (alphabets) !! n
oget n = cycle (others ++ digits) !! n

--Redirections
put0Inm :: Int -> [a] -> [a]
put0Inm m l = tail ( fst (splitAt m l) ++ head l : snd (splitAt m l) )-- takes the 0 index elt and puts it in the m index spot
psychlea :: Int -> [Char]  -- code didn't accept [a] instead of [Char] for some reason
psychleo :: Int -> [Char]  
psychlea m = map aget ( map (*m) (map aindex alphabets) )  -- for uniqueness make sure m coprime with 52 = 13*2*2
psychleo m = map oget ( map (*m) (map oindex (others ++ digits) ) )  -- for uniqueness make sure m coprime with 42 = 2*3*7
reco :: Eq a => [a] -> Int -> [a]
reco l m = 
 if (m == 0) then l 
 else reco (put0Inm m l) (m-1) 
swap ...
 if (n > m) then reco (reco l n) m
 else reco (reco l m) n
-}
{- if (l == []) then l 
  else if (n < m) then fst (splitAt n l) ++ [l !! m ] ++ tail ( swap ( snd (splitAt n l) ) 0 (m-n) )
  else if (m < n) then swap l m n
   else l -}
   
--a1 = put0Inm 35 alphabets
--a2 = psychle a1 41 

{- time for some nerd shit: are the two redirections enough to generate all cyles? 
in other words: do they generate Sn for all n?
actually nvm, obviously yes... just noticed that put0Inm alone is enough for that. 
any 2-cycle (n m) is the same as 
ok so I spent probably over an hour now doing group theory and I'm afraid continuing this will make me hate myself because I've wasted time on something that has no obvious pro and many obvious cons
hence I'll just use a swap function directly. 
-}

-- Making of the "randomized" lists:
index :: Eq a => [a] -> a  -> Int
index l x = length $ takeWhile (/= x)  l

get :: Eq a => [a] -> Int  ->  a 
get l n = cycle l !! n 

psychle :: Eq a => [a] -> Int -> [a]  -- usual GT *m of a cyclic group
psychle l m = map (get l ) ( map (*m) (map (index l ) l ) )

swap :: Eq a => [a] -> Int -> Int -> [a]  -- usual group theory 2-cycle
swap l n m = 
 if (n<m) then 
  fst(splitAt n l) ++ (l !! m) :  tail ( fst(splitAt (m-n) (snd(splitAt n l)) ) ) ++ (l !! n) : tail (snd(splitAt (m-n) (snd(splitAt n l)) ) )
 else swap l m n

-- can make any element of Sn complete. 

{-
a1 = swap alphabets 0 37  -- moving A and replacing it with whatever 37 is
a2 = psychle a1 41 -- now I don't know off the top of my head what it's gonna be. 
a3 = swap a2 13 50
a4 = psychle (swap a3 0 31) 11 --Looks random enough.
arandom = "XjSBkTCAUDmVEnNFolGpYHqZIraJsbKtcLudMveWwfOxgPyhQziR"
o1 = swap (others ++ digits) 0 25
o2 = psychle o1 19
o3 = swap o2 1 13
o4 = psychle o3 17
orandom = "^|;$05'3[*6!-9{:#~=&2@)>],8`/\"}<%1?(4\\+7_."
newcharacters = orandom ++ arandom  -- length 94 = 2*47
n1 = swap newcharacters 0 77
n2 = psychle n1 43
n3 = swap n2 53 81
n4 = psychle n3 63 
n1' = swap ( ' ' : n4 ) 0 51 -- length 95 = 5*19
n2' = psychle n1' 36
n3' = swap n2' 4 44
n4' = psychle n3' 83
nrandom = "d^Gj8*gJm(=|MpS`6PsV4&;vYB/!ybE\\2$eHk\"-hKn+@0WqT}9QtN7)>wZC<{zcF_5'fIA%:iLo.]3OrU1#RulX,[xaD?~"
-}


nrandom = "Q+l$]Yf`Cg9?KR\\o;5pw8Ta-1bi4}|)GW[kr!%sz(Nd@Xe3BI6<J ~nu2.v'SZ*FDh#EL&_M>jqx\"Uy:Vc=7^0,HO/AP{mt" -- was nrandom'

encrypt1 :: [Char] -> [Char]
decrypt1 :: [Char] -> [Char]
encrypt1 a = map (get nrandom ) (map (index karakters) a)
decrypt1 a = map (get karakters ) (map (index nrandom) a)

-- Now to make it with a password:
pass = "AriGaT0r" -- Later we make it so that the user can input the password. 
-- nprandom = 


{-
-- is it random enough?                   ===> LATER
v1 = map (index alphabets ) arandom
v2 = map (index alphabets ) alphabets
v = zipWith (-) v1 v2
vdotv = sum (zipWith (*) v v)
normv = sqrt (fromIntegral vdotv)
-}

-- why doesn't the $ thing work? 
-- how can I use map with a multivariate function when I've filled in n-1 of the variables?  
--   ==> can use map like that if the outermost variable is the one left blank! so either define a new function with the order of the variables changed or use functions on functions like flip

--generator1 :: Char -> Char
--generator1 a = 

-- C:\Coding\Cipher.hs