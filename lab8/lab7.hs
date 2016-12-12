-- Informatics 1 - Functional Programming
-- Tutorial 7
--
-- Week 9 - Due: 19/20 Nov.


import LSystem
import Test.QuickCheck

-- Exercise 1

-- 1a. split
split :: Command -> [Command]
split Sit = []
split (c1 :#: c2) = split c1 ++ split c2
split c = [c]

-- 1b. join
join :: [Command] -> Command
join = foldr (:#:) Sit

-- 1c  equivalent
equivalent :: Command -> Command -> Bool
equivalent a b = (split a) == (split b)

-- 1d. testing join and split
prop_split_join c = equivalent c (join (split c))

prop_split c = (split c) == (filter (elimStuff) (split c))
    where elimStuff :: Command -> Bool
          elimStuff Sit = False
          elimStuff (a:#:b) = False
          elimStuff c = True


-- Exercise 2
-- 2a. copy
copy :: Int -> Command -> Command
copy 0 c = Sit
copy nr c = c :#: (copy (nr-1) c)

-- 2b. pentagon
pentagon :: Distance -> Command
pentagon dist = copy 5 (Go dist :#: Turn 72)

-- 2c. polygon
polygon :: Distance -> Int -> Command
polygon dist nr = copy nr (Go dist :#: Turn (  (360 / fromIntegral nr)))



-- Exercise 3
-- spiral
spiral :: Distance -> Int -> Distance -> Angle -> Command
spiral side 0 step angle = Sit
spiral side n step angle
    | side > 0 = (Go side :#: Turn angle) :#: (spiral (side+step) (n-1) step angle)
    | side <= 0 = Sit

-- Exercise 4
-- optimise
comandaS = (Go 10 :#: Sit :#: Go 20 :#: Turn 35 :#: Go 0 :#: Turn 15 :#: Turn (-50))

optimise :: Command -> Command
optimise c
  | (equivalent c (join (compress (filter (noNulls) (split c))))) == True = trimSit c
  | (equivalent c (join (compress (filter (noNulls) (split c))))) == False = optimise (join (compress (filter (noNulls) (split c))))
    where trimSit (c:#:Sit) = c
          trimSit c = c
          noNulls :: Command -> Bool
          noNulls (Go 0) = False
          noNulls (Turn 0) = False
          noNulls Sit = False
          noNulls _ = True
          compress :: [Command] -> [Command]
          compress [] = []
          compress ((Turn x):(Turn y):xs) = compress ([Turn (x+y)] ++ xs)
          compress ((Go x):(Go y):xs) = compress ([Go (x+y)] ++ xs)
          compress (x:xs) = [x] ++ (compress xs)



-- L-Systems

-- 5. arrowhead
arrowhead :: Int -> Command
arrowhead x = f x
    where f 0 = Go 10
          f x = (g (x-1)) :#: p :#: (f (x-1)) :#: p :#: (g (x-1))
          g 0 = Go 10
          g x = (f (x-1)) :#: n :#: (g (x-1)) :#: n :#: (f (x-1))
          p = Turn 60
          n = Turn (-60)

-- 6. snowflake
snowflake :: Int -> Command
snowflake x = f (x-1) :#: n :#: p :#: f (x-1) :#: n :#: n :#: f (x-1) :#: n :#: n
    where f 0 = Go 10
          f x = f (x-1) :#: p :#: f (x-1) :#: n :#: n :#: f (x-1) :#: p :#: f (x-1)
          p = Turn 60
          n = Turn (-60)

-- 7. hilbert
hilbert :: Int -> Command
hilbert x = l x
    where p = Turn 90
          n = Turn (-90)
          l 0 = Go 10
          l x = p :#: r (x-1) :#: f (x-1) :#: n :#: l (x-1) :#: f (x-1) :#: l (x-1) :#: n :#: f (x-1) :#: r (x-1) :#: p
          r 0 = Go 10
          r x = n :#: l (x-1) :#: f (x-1) :#: p :#: r (x-1) :#: f (x-1) :#: r (x-1) :#: p :#: f (x-1) :#: l (x-1) :#: n
          f 0 = Go 10
          f x = p :#: f (x-1) :#: n

main :: IO ()
main = display (Go 30 :#: Turn 120 :#: Go 30 :#: Turn 120 :#: Go 30)
