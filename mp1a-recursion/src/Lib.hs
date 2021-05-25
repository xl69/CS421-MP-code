--- xl69 tianhui2
--- Getting Started
--- ===============

--- Relevant Files
--- --------------

module Lib where

-- This line imports the Prelude module without certain functions
import Prelude hiding ( take, drop, reverse
                      , zip, zipWith
                      , map, foldl, foldr
                      , iterate, repeat
                      , replicate, cycle
                      , (++)
                      )
-- When you are allowed to use builtin functions Prepend them with "P."
-- for example `P.take`
import qualified Prelude as P

--- Problems
--- ========

--- Recursion
--- ---------

--- ### mytake

-- don't forget to put the type declaration or you will lose points!
mytake :: Int -> [a] -> [a]
mytake n l | n <= 0 = []
mytake n [] = []
mytake n (x:xs) = x : mytake (n-1) xs

--- ### mydrop

-- don't forget to put the type declaration or you will lose points!
mydrop :: Int -> [a] -> [a]
mydrop n l | n <= 0 = l
mydrop n [] = []
mydrop n (x : xs) = mydrop (n-1) xs

--- ### rev

-- don't forget to put the type declaration or you will lose points!
rev :: [a] -> [a]
rev [] = []
rev (x:xs) = addToTail (x:xs) []
             where
              addToTail [] a  = a
              addToTail (x:xs) a = addToTail xs (x:a)

--- ### app

-- don't forget to put the type declaration or you will lose points!
app :: [a] -> [a] -> [a]
app list [] = list
app [] list = list
app (x : xs) list = x : app xs list

--- ### inclist

-- don't forget to put the type declaration or you will lose points!
inclist :: Num a => [a] -> [a]
inclist [] = []
inclist (x:xs) = x+1 : inclist xs

--- ### sumlist

-- don't forget to put the type declaration or you will lose points!
sumlist :: Num a => [a] -> a 
sumlist [] = 0
sumlist (x:xs) = x + sumlist xs

--- ### myzip

-- don't forget to put the type declaration or you will lose points!
myzip :: [a] -> [b] -> [(a,b)]
myzip l [] = []
myzip [] l = []
myzip (x:xs) (y:ys) = (x,y) : myzip xs ys

--- ### addpairs

-- don't forget to put the type declaration or you will lose points!
addpairs :: (Num a) => [a] -> [a] -> [a]
addpairs first second = redundant (myzip first second)
                        where
                          redundant [] = []
                          redundant ((x1, x2) : xs) = (x1 + x2) : redundant xs

--- ### ones

-- don't forget to put the type declaration or you will lose points!
ones :: [Integer]
ones = repeat 1
    where repeat x = x : repeat x

--- ### nats

-- don't forget to put the type declaration or you will lose points!
nats :: [Integer] 
nats = [0..]

--- ### fib

-- don't forget to put the type declaration or you will lose points!
fib  :: [Integer]
fib = 0 : 1 : addpairs fib (tail fib)

--- Set Theory
--- ----------

--- ### add

-- don't forget to put the type declaration or you will lose points!
add :: Ord a => a -> [a] -> [a]
add x [] = [x]
add x (y:ys) | x == y = (y:ys) | x > y = y : add x ys | x < y = x : y : ys

--- ### union

-- don't forget to put the type declaration or you will lose points!
union :: Ord a => [a] -> [a] -> [a]
union [] [] = []
union list [] = list
union [] list = list
union (x:xs) (y:ys) | x == y = x : union xs ys | x < y = x : union xs (y:ys) | x > y = y : union (x:xs) ys

--- ### intersect

-- don't forget to put the type declaration or you will lose points!
intersect :: Ord a => [a] -> [a] -> [a]
intersect [] [] = []
intersect list [] = []
intersect [] list = []
intersect (x:xs) (y:ys) | x == y = x : intersect xs ys | x < y = intersect xs (y:ys) | x > y = intersect (x:xs) ys

--- ### powerset

-- don't forget to put the type declaration or you wi(x:xs) = union xss (map (x:) xss)ll lose points!
powerset :: Ord a => [a] -> [[a]]
powerset [] = [[]]
powerset (x:xs) = power (x:xs) [] 
                  where 
                    power [] set = [set] 
                    power (x:xs) set = union (power xs (add x set)) (power xs set)


--- Higher Order Functions
--- ----------------------

--- ### inclist'

-- don't forget to put the type declaration or you will lose points!
inclist' :: Num a => [a] -> [a] 
inclist' list = P.map (+1) list
        
--- ### sumlist'

-- don't forget to put the type declaration or you will lose points!
sumlist' :: (Num a) => [a] -> a
sumlist' list = P.foldl (+) 0 list 
