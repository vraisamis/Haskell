module ProjectEuler0xx
( problem001
, problem002
, problem003
) where

problem001 :: Int
problem001 = foldl (+) 0 xs
    where xs = [x | x <- [1..999], x `mod` 3 == 0 || x `mod` 5 == 0]

fibs :: Int ->[Integer] -> [Integer]
fibs n [] = fibs (n - 2) [2, 1]
fibs n (x:y:xs)
    | n > 0 = fibs (n - 1) ((x+y):x:y:xs)
    | otherwise = x:y:xs

problem002 :: Integer
problem002 = foldl (+) 0 $ filter (\x -> x `mod` 2 == 0) $ takeWhile (<4000000) fibonacci
    where fibonacci = reverse $ fibs 10000 []

problem003 :: Integer
problem003 = 600851475143
