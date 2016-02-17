module ProjectEuler0xx
( problem001
, problem002
, problem003
, problem004
, problem005
, problem006
, problem007
) where

-- import Data.List

problem001 :: Int
problem001 = foldl (+) 0 xs
    where xs = [x | x <- [1..999], x `mod` 3 == 0 || x `mod` 5 == 0]

fibs :: [Integer]
fibs = 1:2:zipWith (+) fibs (tail fibs)

problem002 :: Integer
problem002 = foldl (+) 0 $ filter odd $ takeWhile (<4000000) fibs

problem003 :: Integer
problem003 = foldl max 1 [x | x <- [1..n], n `mod` x == 0]
    where n = 600851475143

problem004 :: Integer
problem004 = head [x*y | x <- reverse [100..999], y <- reverse [100..999], (reverse $ show (x*y)) == (show (x*y))]

problem005 :: Integer
problem005 = foldl lcm 1 [2..20]
    where lcm n m = (n * m) `div` (gcd n m)

problem006 :: Integer
problem006 = let sumsq = (^2) $ foldl (+) 0 xs
                 sqsum = foldl (+) 0 $ map (^2) xs
             in sumsq - sqsum 
             where xs = [1..100]

primes = p [2..]
    where p (x:xs) = x:(p $ filter (\y -> y `mod` x /= 0) xs)

problem007 :: Integer
problem007 = primes !! 10001

myjoin [] = []
myjoin [[]] = []
myjoin ([]:xss) = myjoin xss
myjoin ((x:xs):xss) = x:(myjoin $ xss ++ [xs])

primes' = 2:3:5:(p $ tail $ myjoin [
        [ 1,  1 + 30..],
        [ 7,  7 + 30..],
        [11, 11 + 30..],
        [13, 13 + 30..],
        [17, 17 + 30..],
        [19, 19 + 30..],
        [23, 23 + 30..],
        [29, 29 + 30..]])
    where p (x:xs) = x:(p $ filter (\y -> y `mod` x /= 0) xs)

problem007' = primes' !! 10001

str008 :: String
str008="7316717653133062491922511967442657474235534919493496983520312774506326239578318016984801869478851843858615607891129494954595017379583319528532088055111254069874715852386305071569329096329522744304355766896648950445244523161731856403098711121722383113622298934233803081353362766142828064444866452387493035890729629049156044077239071381051585930796086670172427121883998797908792274921901699720888093776657273330010533678812202354218097512545405947522435258490771167055601360483958644670632441572215539753697817977846174064955149290862569321978468622482839722413756570560574902614079729686524145351004748216637048440319989000889524345065854122758866688116427171479924442928230863465674813919123162824586178664583591245665294765456828489128831426076900422421902267105562632111110937054421750694165896040807198403850962455444362981230987879927244284909188845801561660979191338754992005240636899125607176060588611646710940507754100225698315520005593572972571636269561882670428252483600823257530420752963450"

multi13 :: String -> Integer
multi13 [] = 0
multi13 (x:xs) = max (multi13 xs) $ foldl (*) 1 $ map (\x -> (read [x]::Integer)) $ take 13 $ x:xs

problem008 :: Integer
problem008 = multi13 str008

problem009 = head [a * b * c | a <- [1..1000], b <- [1..a], c <- [1..b], a + b + c == 1000, a^2 == b^2 + c^2]

problem010 = sum $ takeWhile (<=2000000) primes

productions x = [a | a <- [1..x], x `mod` a == 0]

triangles = 1:zipWith (+) [2..] triangles

problem012 = head $ filter (\x -> (length $ productions x) > 500) triangles
