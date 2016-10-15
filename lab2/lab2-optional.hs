-- Declarative Programming
-- Lab 2
--

import Data.Char
import Data.List
import Test.QuickCheck


-- Optional Material

-- 8. crosswordFind

-- List-comprehension version
checkWord :: Char -> Int -> String -> Bool
checkWord letter inP word = if length(word) > inP && inP >= 0
  then letter == word!!inP else False

crosswordFind :: Char -> Int -> Int -> [String] -> [String]
crosswordFind letter inP len wordz =
  [x | x<-wordz, length(x) == len && (checkWord letter inP x)]

-- Recursive version
crosswordFindRec :: Char -> Int -> Int -> [String] -> [String]
crosswordFindRec letter inP len [] = []
crosswordFindRec letter inP len (word:wordz) =
  if length(word) == len && (checkWord letter inP word)
    then word : (crosswordFindRec letter inP len wordz)
    else (crosswordFindRec letter inP len wordz)

-- Mutual test
prop_crosswordFind :: Char -> Int -> Int -> [String] -> Bool
prop_crosswordFind letter inP len wordz =
  (crosswordFindRec letter inP len wordz)
  == (crosswordFind letter inP len wordz)



-- 9. search

-- List-comprehension version
getIndex :: ([Char], [Int]) -> Int
getIndex (c, i) = i!!0
getLetter :: ([Char], [Int]) -> Char
getLetter (c, i) = c!!0

search :: String -> Char -> [Int]
search word letter =
  [ getIndex (unzip [y]) | y <-(zip word [0..length(word)-1]),
   getLetter(unzip[y]) == letter]

-- Recursive version
searchRec2 :: String -> Char -> Int -> [Int]
searchRec2 [] letter index = []
searchRec2 (w:word) letter index = if w == letter
   then index : (searchRec2 word letter (index+1)) else (searchRec2 word letter (index+1))

searchRec :: String -> Char -> [Int]
searchRec [] letter = []
searchRec word letter = searchRec2 word letter 0

-- Mutual test
prop_search :: String -> Char -> Bool
prop_search word letter = search word letter == searchRec word letter

-- 10. contains

-- List-comprehension version
word = "United Kingdom"
lista = [drop x word | x<-[0..length(word)]]

contains :: String -> String -> Bool
contains first second =length([ y |y <- [drop x first | x<-[0..length(first)]], (isPrefixOf second y)]) > 0

-- Recursive version
containsRec :: String -> String -> Bool
containsRec [] [] = True
containsRec [] second = False
containsRec (f:first) second = if isPrefixOf second (f:first) then True else containsRec first second

-- Mutual test
prop_contains :: String -> String -> Bool
prop_contains first second = contains first second == containsRec first second
