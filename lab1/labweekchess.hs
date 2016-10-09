-- Informatics 1 - Functional Programming
-- Lab week tutorial part II
--
--

import PicturesSVG
import Test.QuickCheck
import Data.Char



-- Exercise 8:

pic1 :: Picture
pic1 = above (beside knight (invert knight)) (beside (invert knight) knight)

pic2 :: Picture
pic2 = above (beside knight (invert knight)) (beside (invert (flipV knight)) (flipV knight))


-- Exercise 9:
-- a)

emptyRow :: Picture
emptyRow = repeatH 4 (beside whiteSquare blackSquare)

-- b)

otherEmptyRow :: Picture
otherEmptyRow = repeatH 4 (beside blackSquare whiteSquare)

-- c)

middleBoard :: Picture
middleBoard = repeatV 2 (above emptyRow otherEmptyRow)

-- d)
--This array contains the pictures in the proper order
pictureRow = [rook,knight,bishop,queen,king,bishop,knight]

chessComplex :: Picture
chessComplex = foldl (beside) (take 1 pictureRow!!0) (reverse pictureRow)

whiteRow :: Picture
whiteRow = over chessComplex otherEmptyRow

blackRow :: Picture
blackRow = over (invert chessComplex) emptyRow

-- e)

whitePawn :: Picture
whitePawn = over (repeatH 8 pawn) emptyRow

blackPawn :: Picture
blackPawn = over (repeatH 8 (invert pawn)) otherEmptyRow

populatedBoard :: Picture
populatedBoard = foldl (above) blackRow [blackPawn, middleBoard, whitePawn, whiteRow]



-- Functions --

twoBeside :: Picture -> Picture
twoBeside x = beside x (invert x)


-- Exercise 10:

twoAbove :: Picture -> Picture
twoAbove x = above x (invert x)

fourPictures :: Picture -> Picture
fourPictures x = twoAbove (twoBeside x)
