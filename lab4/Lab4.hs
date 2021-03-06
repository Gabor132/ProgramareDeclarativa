-- Informatics 1 - Functional Programming
-- Tutorial 3
--
-- Week 5 - Due: 22/23 Oct.

import Data.Char
import Test.QuickCheck



-- 1. Map
-- a. (4 simboluri)
uppers :: String -> String
uppers xs = map toUpper xs

-- b. (7 simboluri)
doubles :: [Int] -> [Int]
doubles = map (\x -> x*2)

-- c. (10 simboluri)
penceToPounds :: [Int] -> [Float]
penceToPounds = map (\x -> (fromIntegral x)/100)

-- d. (11 simboluri)
uppers' :: String -> String
uppers' xs = [toUpper x | x<-xs]

-- (8 simboluri)
prop_uppers :: String -> Bool
prop_uppers xs = uppers' xs == uppers xs



-- 2. Filter
-- a. (4 simboluri)
alphas :: String -> String
alphas xs = filter isAlpha xs

-- b. (8 simboluri)
rmChar ::  Char -> String -> String
rmChar c xs = filter (different) xs
            where different a = a /= c

-- c. (8 simboluri)
above :: Int -> [Int] -> [Int]
above i xs = filter less xs
            where less a = a > i

-- d. (13 simboluri)
unequals :: [(Int,Int)] -> [(Int,Int)]
unequals xs = filter (different2) xs
          where different2 (a,b) = a /= b

-- e. (15 simboluri)
rmCharComp :: Char -> String -> String
rmCharComp c xs = [x | x<-xs, c /= x]

-- (11 simboluri)
prop_rmChar :: Char -> String -> Bool
prop_rmChar c xs = rmCharComp c xs == rmChar c xs



-- 3. Comprehensions vs. map & filter
-- a.
upperChars :: String -> String
upperChars s = [toUpper c | c <- s, isAlpha c]

-- (7 simboluri)
upperChars' :: String -> String
upperChars' xs = map (toUpper) (filter (isAlpha) xs)

prop_upperChars :: String -> Bool
prop_upperChars s = upperChars s == upperChars' s

-- b.
largeDoubles :: [Int] -> [Int]
largeDoubles xs = [2 * x | x <- xs, x > 3]

-- (13 simboluri)
largeDoubles' :: [Int] -> [Int]
largeDoubles' x = map (*2) (filter (>3) x)

prop_largeDoubles :: [Int] -> Bool
prop_largeDoubles xs = largeDoubles xs == largeDoubles' xs

-- c.
reverseEven :: [String] -> [String]
reverseEven strs = [reverse s | s <- strs, even (length s)]

-- (11 simboluri)
reverseEven' :: [String] -> [String]
reverseEven' xs = map (reverse) (filter (isEven) xs)
              where isEven a = even (length a)

prop_reverseEven :: [String] -> Bool
prop_reverseEven strs = reverseEven strs == reverseEven' strs



-- 4. Foldr
-- a.
productRec :: [Int] -> Int
productRec []     = 1
productRec (x:xs) = x * productRec xs

-- (7 simboluri)
productFold :: [Int] -> Int
productFold x = foldr (*) 1 x

prop_product :: [Int] -> Bool
prop_product xs = productRec xs == productFold xs

-- b.  (16 simboluri)
andRec :: [Bool] -> Bool
andRec [] = True
andRec (x:xs) = x && andRec xs

-- (7 simboluri)
andFold :: [Bool] -> Bool
andFold = foldr (&&) True

prop_and :: [Bool] -> Bool
prop_and xs = andRec xs == andFold xs

-- c.  (17 simboluri)
concatRec :: [[a]] -> [a]
concatRec [] = []
concatRec (x:xs) = x ++ (concatRec xs)

-- (8 simboluri)
concatFold :: [[a]] -> [a]
concatFold = foldr (++) []

prop_concat :: [String] -> Bool
prop_concat strs = concatRec strs == concatFold strs

-- d.  (17 simboluri)
rmCharsRec :: String -> String -> String
rmCharsRec [] b = b
rmCharsRec (x:a) b = rmCharsRec a (rmChar x b)

-- (6 simboluri)
rmCharsFold :: String -> String -> String
rmCharsFold a b = foldr (rmChar) b a

prop_rmChars :: String -> String -> Bool
prop_rmChars chars str = rmCharsRec chars str == rmCharsFold chars str



type Matrix = [[Int]]


-- 5
-- a. (10 simboluri)
uniform :: [Int] -> Bool
uniform (x:xs)= all (==x) xs

-- b. (	 simboluri)
valid :: Matrix -> Bool
valid [] = False
valid [[]] = False
valid (x:xs) = all (eqLength x) xs
              where eqLength a b = length a == length b

-- 6.

-- 7.  (22 simboluri + 19 simboluri)  cu tot cu tratarea erorilor
plusRow :: [Int] -> [Int] -> [Int]
plusRow (a:x) (b:y)
  | length(a:x) /= length(b:y) = error "NOT THE SAME DIMENSIONS"
  | otherwise = a+b : plusRow x y
plusRow [] [] = []
plusRow a b = error "NOT VALID DIMENSIONS"
plusM :: Matrix -> Matrix -> Matrix
plusM (a:m) (b:n)
  | length (a:m) /= length (b:n)  = error "NOT THE SAME DIMENSIONS"
  | otherwise = plusRow a b : plusM m n
plusM [] [] = []
plusM a b = error "NOT VALID DIMENSIONS"

-- 8. (23 simboluri + 15 simboluri)  cu tot cu tratarea erorilor
dotProduct :: [Int] -> [Int] -> Int
dotProduct a b = foldr (+) 0 (map (product) (zip a b))
  where product x = uncurry (*) x

timesM :: Matrix -> Matrix -> Matrix
timesM = undefined
