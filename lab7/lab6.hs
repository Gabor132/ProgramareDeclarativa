-- Informatics 1 Functional Programming
-- Tutorial 6
--
-- Due: 12/13 November

import System.Random


-- Importing the keymap module

import KeymapTree

-- Type declarations

type Barcode = String
type Product = String
type Unit    = String

type Item    = (Product,Unit)

type Catalogue = Keymap Barcode Item


-- A little test catalog

testDB :: Catalogue
testDB = fromList [
 ("0265090316581", ("The Macannihav'nmor Highland Single Malt", "75ml bottle")),
 ("0903900739533", ("Bagpipes of Glory", "6-CD Box")),
 ("9780201342758", ("Thompson - \"Haskell: The Craft of Functional Programming\"", "Book")),
 ("0042400212509", ("Universal deep-frying pan", "pc"))
 ]

-- Exercise 1

longestProductLen :: [(Barcode, Item)] -> Int
longestProductLen lista = maximum [length nume | (cod,(nume, cant))<-lista]

formatLine :: Int -> (Barcode, Item) -> String
formatLine i (cod,(nume,cant)) = cod ++ "..." ++ nume ++ (punct i (length nume)) ++ cant
    where punct i j = foldr (++) "" (take (i-j) (repeat "."))

showCatalogue :: Catalogue -> String
showCatalogue c = foldr (format) "" (map (formatLine (longestProductLen (toList c))) (toList c))
    where format a b = a ++ " \n " ++b
-- Exercise 2
maybeToList :: Maybe Item -> [Item]
maybeToList Nothing = []
maybeToList (Just x) = [x]

listToMaybe :: [Item] -> Maybe Item
listToMaybe [] = Nothing
listToMaybe (x:xs) = Just x

catMaybes :: [Maybe Item] -> [Item]
catMaybes xs = map (stripJ) (filter (stripN) xs)
  where stripN Nothing = False
        stripN (Just x) = True
        stripJ (Just x) = x

-- Exercise 3
list :: [Barcode]
list = ["0001","9780201342758","0003"]

getItems :: [Barcode] -> Catalogue -> [Item]
getItems [] c = []
getItems (x:xs) c = (catMaybes [(get x c)]) ++ (getItems xs c)






-- Input-output ------------------------------------------

readDB :: IO Catalogue
readDB = do dbl <- readFile "database.csv"
            let db = fromList (map readLine $ lines dbl)
            putStrLn (size db >= 0 `seq` "Done")
            return db

readLine :: String -> (Barcode,Item)
readLine str = (a,(c,b))
    where
      (a,str2) = splitUpon ',' str
      (b,c)    = splitUpon ',' str2

splitUpon :: Char -> String -> (String,String)
splitUpon _ "" = ("","")
splitUpon c (x:xs) | x == c    = ("",xs)
                   | otherwise = (x:ys,zs)
                   where
                     (ys,zs) = splitUpon c xs

getSample :: Catalogue -> IO Barcode
getSample db = do g <- newStdGen
                  return $ fst $ toList db !! fst (randomR (0,size db - 1) g)
