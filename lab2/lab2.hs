-- Declarative Programming
-- Lab 2
--

import Data.Char
import Data.List
import Test.QuickCheck



-- 1. halveEvens

isEven :: Int -> Bool
isEven x = if mod x 2 == 0 then True else False

-- List-comprehension version
halveEvens :: [Int] -> [Int]
halveEvens xs = [div x 2 | x <- xs, isEven x]
--if isEven ((take 1 xs)!!0) then take 1 xs : halveEvens reverse(take length(xs)-1 reverse(xs)) else halveEvens reverse(take length(xs)-1 reverse(xs))

-- Recursive version
halveEvensRec :: [Int] -> [Int]
halveEvensRec [] = []
halveEvensRec (x:xs) =
    if isEven x
      then (div x 2) : halveEvensRec  xs
      else halveEvensRec xs

-- Mutual test
prop_halveEvens :: [Int] -> Bool
prop_halveEvens xs = halveEvens xs == halveEvensRec xs



-- 2. inRange

-- List-comprehension version
inRange :: Int -> Int -> [Int] -> [Int]
inRange lo hi xs = [x | x <- xs, x >= lo && x <= hi]

-- Recursive version
inRangeRec :: Int -> Int -> [Int] -> [Int]
inRangeRec lo hi [] = []
inRangeRec lo hi (x:xs) =
  if ((x >= lo) && (x <= hi))
      then x : inRangeRec lo hi xs
      else inRangeRec lo hi xs


-- Mutual test
prop_inRange :: Int -> Int -> [Int] -> Bool
prop_inRange lo hi xs = inRange lo hi xs == inRangeRec lo hi xs



-- 3. sumPositives: sum up all the positive numbers in a list

-- List-comprehension version
countPositives :: [Int] -> Int
countPositives xs = length [ x | x<-xs , x>0 ]

-- Recursive version
countPositivesRec :: [Int] -> Int
countPositivesRec [] = 0
countPositivesRec (x:xs) = if x>0 then 1+countPositivesRec xs else countPositivesRec xs

-- Mutual test
prop_countPositives :: [Int] -> Bool
prop_countPositives xs = countPositives xs == countPositivesRec xs



-- 4. pennypincher

-- Helper function
discount :: Int -> Int
discount x = round (fromIntegral x*0.9)

-- List-comprehension version
pennypincher :: [Int] -> Int
pennypincher xs = sum [discount x | x<-xs, discount x <= 19900]

-- Recursive version
pennypincherRec :: [Int] -> Int
pennypincherRec [] = 0
pennypincherRec (x:xs) = if discount x <= 19900
  then discount x + pennypincherRec xs
  else pennypincherRec xs

-- Mutual test
prop_pennypincher :: [Int] -> Bool
prop_pennypincher xs = pennypincherRec xs == pennypincher xs



-- 5. sumDigits

-- List-comprehension version
multDigits :: String -> Int
multDigits s = product [ digitToInt x | x<-s, isDigit x]

-- Recursive version
multDigitsRec :: String -> Int
multDigitsRec [] = 1
multDigitsRec (c:s) = if isDigit c then (digitToInt c) *(multDigitsRec s) else multDigitsRec s

-- Mutual test
prop_multDigits :: String -> Bool
prop_multDigits s = multDigits s == multDigitsRec s



-- 6. capitalise

-- List-comprehension version
capitalise :: String -> String
capitalise [] = []
capitalise (y:s) = (toUpper y) : [toLower x | x<-s]

-- Recursive version

capitaliseRecB :: String -> String
capitaliseRecB [] = []
capitaliseRecB (y:s) = toLower y  : capitaliseRecB s

capitaliseRec :: String -> String
capitaliseRec [] = []
capitaliseRec (y:s) = toUpper y : capitaliseRecB s

-- Mutual test
prop_capitalise :: String -> Bool
prop_capitalise s = capitalise s == capitaliseRec s



-- 7. title

-- List-comprehension version

lowerAll :: String -> String
lowerAll xs = [toLower x | x<-xs]

title :: [String] -> [String]
title [] = []
title (x:s) = capitalise x : [if length(y)>=4 then capitalise y else lowerAll y | y<-s]

-- Recursive version
lowerAllRec :: String -> String
lowerAllRec [] = []
lowerAllRec (x:s) = toLower x : lowerAll s

titleRec2 :: [String] -> [String]
titleRec2 [] = []
titleRec2 (x:s) = if length(x) >= 4
  then capitaliseRec x : titleRec2 s
  else lowerAllRec x : titleRec2 s

titleRec :: [String] -> [String]
titleRec [] = []
titleRec (x:s) = capitaliseRec x : titleRec2 s

-- mutual test
prop_title :: [String] -> Bool
prop_title xs = titleRec xs == title xs
