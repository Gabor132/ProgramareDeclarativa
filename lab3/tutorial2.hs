-- Informatics 1 - Functional Programming
-- Tutorial 2
--
-- Week 4 - due: 11/12 Oct.

import Data.Char
import Data.List
import Test.QuickCheck

-- 1.
alfabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
rotate :: Int -> [Char] -> [Char]
rotate ciclu alfabet = if ciclu >=0 && ciclu < length(alfabet)
  then concat [(drop ciclu alfabet), (take ciclu alfabet)]
  else error "INPUT INVALID"
-- 2.
prop_rotate :: Int -> String -> Bool
prop_rotate k str = rotate (l - m) (rotate m str) == str
                        where l = length str
                              m = if l == 0 then 0 else k `mod` l

-- 3.
makeKey :: Int -> [(Char, Char)]
makeKey x = zip alfabet (rotate x alfabet)

-- 4.
getKey :: (Char, Char) -> Char
getKey (x, y) = x

getValue :: (Char, Char) -> Char
getValue (x, y) = y

parcurg :: Char -> [(Char, Char)] -> [Char]
parcurg l cypher = [getValue x | x<-cypher, getKey x == l]


lookUp :: Char -> [(Char, Char)] -> Char
lookUp l cypher = if length(parcurg l cypher) > 0
   then (parcurg l cypher)!!0
   else l

-- 5.
encipher :: Int -> Char -> Char
encipher i c = lookUp c (makeKey i)

-- 6.
normalize :: String -> String
normalize xs = [toUpper x | x<-xs, isDigit x || isAlpha x]

-- 7.
encipherStr :: Int -> String -> String
encipherStr x text = [encipher x c|c <- (normalize text)]

-- 8.
reverseKey :: [(Char, Char)] -> [(Char, Char)]
reverseKey key = [(zip ([getValue x]) ([getKey x]))!!0|x<-key]

-- 9.
decipher :: Int -> Char -> Char
decipher i c = lookUp c (reverseKey (makeKey i))

decipherStr :: Int -> String -> String
decipherStr i code = [decipher i c |c<-(normalize code)]

-- 10.
prop_cipher :: Int -> String -> Property
prop_cipher i text = (i >= 0 && i < 26)
  ==> (decipherStr i (encipherStr i (normalize text))) == normalize text

-- 11.
contains :: String -> String -> Bool
contains first second =length([ y |y <- [drop x first | x<-[0..length(first)]], (isPrefixOf second y)]) > 0

-- 12.
containsSpecial :: String -> Int -> Bool
containsSpecial input i = ((contains (decipherStr i input) "THE") || (contains (decipherStr i input) "AND"))

candidatesRec :: String -> Int -> [(Int, [Char])]
candidatesRec [] x = []
candidatesRec xs 26 = []
candidatesRec input i = if containsSpecial input i
  then ((i, decipherStr i input) : (candidatesRec input (i+1)))
  else (candidatesRec input (i+1))
candidates :: String -> [(Int, [Char])]
candidates [] = []
candidates input = if containsSpecial input 1
  then ((1, decipherStr 1 input) : (candidatesRec input 2))
  else (candidatesRec input 2)



-- Optional Material

-- 13.
splitEachGiven :: Int -> String -> [String]
splitEachGiven i [] = []
splitEachGiven i xs = if length xs > i
  then (take i xs) : splitEachGiven i (drop i xs)
  else [(xs ++ (replicate (i - length xs) 'X'))]

splitEachFive :: String -> [String]
splitEachFive [] = []
splitEachFive xs = if length xs > 5
  then (take 5 xs) : splitEachFive (drop 5 xs)
  else [(xs ++ (replicate (5 - length xs) 'X'))]

-- 14.
prop_transpose :: String -> Bool
prop_transpose input =
  (splitEachFive input)
  == (transpose (transpose (splitEachFive input)))

-- 15.
encrypt :: Int -> String -> String
encrypt i input = concat (transpose(splitEachFive (encipherStr i input)))

-- 16.
decrypt :: Int -> String -> String
decrypt i input = concat (transpose(splitEachGiven (fromIntegral (div (length input) 5)) (decipherStr i input)))

-- Challenge (Optional)

-- 17.
addFreq :: (Char, Int) -> (Char, Int)
addFreq (x,i) = (x,i+1)

getPair :: [(Char, Int)] -> (Char, Int)
getPair (x:xs) w = if ((getKey x) == (getKey w))
  then w
  else (getPair xs w)

countFreqs :: String -> [(Char, Int)]
countFreqs = undefined

-- 18
freqDecipher :: String -> [String]
freqDecipher = undefined
