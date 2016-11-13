
-- RECAPITULARE

incrPos :: [Int] -> [Int]
incrPos a = map (+1) (filter (>0) a)

incrPosRec :: [Int] -> [Int]
incrPosRec [] = []
incrPosRec (x:xs)
  | x > 0 = x+1 : incrPosRec xs
  | otherwise = incrPosRec xs

incrPosIn :: [Int] -> [Int]
incrPosIn xs = [x+1 | x <- xs, x>0]

divisors2 :: Int -> Int -> [Int]
divisors2 x i
  | i > x = []
  | i <= x && (x `mod` i) == 0 = i : divisors2 x (i+1)
  | otherwise = divisors2 x (i+1)

divisors :: Int -> [Int]
divisors x = divisors2 x 1

oddDivisors :: Int -> [Int]
oddDivisors xs = [x | x <- [1..xs], (xs `mod` x)==0, even(x) == False ]

isPerfect :: Int -> Bool
isPerfect x = (foldr (+) 0 (filter(\a -> (x `mod` a == 0))[1..x-1])) == x

perfDiffs :: [Int] -> Bool
perfDiffs a = isPerfect (sum [uncurry (-) x | x <- (zip (tail a) (init a))])

diffsProd :: [Int] -> Int
diffsProd [] = 1
diffsProd [_] = 1
diffsProd (f:s:xs) = (f - s) * diffsProd (s:xs)

signsInterv :: [Int] -> [Char]
signsInterv xs = [sign x | x<-xs, x<10, x>(-10)]
  where sign x = if x>0 then '+' else (if x < 0 then '-' else '0')

evenVowels :: [String] -> Int
evenVowels xs = sum (map (length.filter (`elem` "aeiou")) (filter (even.length) xs))

primeFactors :: Int -> [Int]
primeFactors x = primeFactorsRec x [2..x]
  where primeFactorsRec x (f:factors)
          | x == 1 = []
          | x `mod` f == 0 = f : primeFactorsRec (x `div` f) (f:factors)
          | otherwise = primeFactorsRec x factors
