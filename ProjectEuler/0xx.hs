module ProjectEuler0xx
( problem001
, problem002
, problem003
, problem004
, problem005
) where

import Data.List

problem001 :: Int
problem001 = foldl (+) 0 xs
    where xs = [x | x <- [1..999], x `mod` 3 == 0 || x `mod` 5 == 0]

fibs :: [Integer]
fibs = 1:2:zipWith (+) fibs (tail fibs)

problem002 :: Integer
problem002 = foldl (+) 0 $ filter (\x -> x `mod` 2 == 0) $ takeWhile (<4000000) fibs

problem003 :: Integer
problem003 = foldl max 1 [x | x <- [1..n], n `mod` x == 0]
    where n = 600851475143

problem004 :: Integer
problem004 = head [x*y | x <- reverse [100..999], y <- reverse [100..999], (reverse $ show (x*y)) == (show (x*y))]

problem005 :: Integer
problem005 = foldl lcm 1 [2..20]
    where lcm n m = (n * m) `div` (gcd n m)

