module Euler.Support where

import Data.Char (digitToInt)
import Data.List

--Prime functions

primes :: [Int]
primes = filter isPrime divisorList

divisorList :: [Int]
divisorList = 2:3:5:(tail $ concatMap (\x -> fmap (x+) primesShortList) [0,30..] )
  where
    primesShortList :: [Int]
    primesShortList = [1,7,11,13,17,19,23,29]

-- based on Miller-Rabin primality test
-- Can fail with numbers greater than sqrt 2^63
isPrime :: Int -> Bool
isPrime x
  | x < 5 || even x = x == 2 || x == 3
  | otherwise       = not . or $ fmap compositeTest as
  where
    compositeTest :: Int -> Bool
    compositeTest a = let first = powerMod (a`mod`x) d x
                      in (first /= 1) && (all (/= (x - 1)) . take maxS $ iterate (\n -> (n * n) `mod` x) first)
    as
      | x < 291831              = [126401071349994536]
      | x < 624732421           = [15, 5511855321103177]
      | x < 4759123141          = [2,7,61]
      | x < 1122004669633       = [2,13,23,1662803]
      | x < 2152302898747       = [2,3,5,7,11]
      | x < 3474749660383       = [2,3,5,7,11,13]
      | x < 341550071728321     = [2,3,5,7,11,13,17]
      | x < 3825123056546413051 = [2,3,5,7,11,13,17,19,23]
      | otherwise               = [2,3,5,7,11,13,17,19,23,29,31,37]
    (d, maxS) = factor (x - 1) 0
factor n c = if odd n then (n,c) else factor (n`div`2) (c+1)

getNextPrime :: Int -> Int
getNextPrime x
  | x < 2     = 2
  | even x    = if isPrime (x+1) then x+1 else getNextPrime (x+1)
  | otherwise = if isPrime (x+2) then x+2 else getNextPrime (x+2)

-- Divisibility and Divisor Functions

-- generates a sorted list of divisors
divisors :: Int -> [Int]
divisors x = generateDivisors 1 []
  where
    generateDivisors :: Int -> [Int] -> [Int]
    generateDivisors d upperList
      | d == sqrRoot x  = if isDivisible d x
                            then if d*d == x
                                   then d:upperList
                                   else d:(x`div`d):upperList
                            else upperList
      | isDivisible d x = d:generateDivisors (d+1) ((x`div`d):upperList)
      | otherwise       = generateDivisors (d+1) upperList

properDivisors :: Int -> [Int]
properDivisors = init . divisors

-- True if x is divisible by d
isDivisible :: Int -> Int -> Bool
isDivisible d x = (x `mod` d) == 0

numDivisors :: Int -> Int
numDivisors x
  | x <= 1    = if x == 1 then 1 else 0
  | otherwise = fromIntegral $ 2
              + (2 * (length . filter (\d -> isDivisible d x) $ [2..root]))
              - if root*root == x then 1 else 0
  where
    root = sqrRoot x

-- Digit Functions

digitsToInt :: Integral a => [Int] -> a
digitsToInt = sum . zipWith (*) (iterate (10*) 1) . reverse . fmap fromIntegral

numDigits :: Integral a => a -> Int
numDigits = length . show . toInteger

toDigits :: Integral a => a -> [Int]
toDigits = fmap digitToInt . show . toInteger

reverseDigits :: Integral a => a -> a
reverseDigits = digitsToInt . reverse . toDigits

-- General Math Functions

-- Factorial grows very quickly. Use type Integer for n > 20
factorial :: Integral a => a -> a
factorial n = product [1..n]

-- (b^e) mod m, without a stack overflow. The optimizations assume that
-- m <= 3037000499.
powerMod :: Integral a => a -> a -> a -> a
powerMod b e m = (powerMod' b e m) `mod` m
  where
    powerMod' b 1 m = shorten b m
    powerMod' b e m = let (e', bit) = e `divMod` 2 -- maybe change to e div 2, and odd e on next line
                          b'        = shorten b m
                      in if bit == 1
                           then shorten (b' * powerMod' (b' * b') e' m) m
                           else      powerMod' (b' * b') e' m
    shorten a m = if a > 3037000499 then a `mod` m else a

-- equivalent to sum [1..n]
sum1ToN :: Int -> Int
sum1ToN n = n*(n+1) `div` 2

sqrRoot :: Int -> Int
sqrRoot = floor . sqrt . fromIntegral

-- Number Tests

isPalindrome :: (Integral a, Show a) => a -> Bool
isPalindrome x
  | x < 0 = False
  | otherwise = (show x) == reverse (show x)

isPandigital :: Integral a => a -> Bool
isPandigital x = [1..9] == sort (toDigits x)

isTriangular :: Int -> Bool
isTriangular x = let a = sqrRoot (2*x)
                 in x == (a*a + a) `div` 2

isSquare :: Int -> Bool
isSquare x = let r = sqrRoot x
             in x == r*r

isPentagonal :: Int -> Bool
isPentagonal x = let a = (ceiling . sqrt . (/1.5) . fromIntegral $ x)
                 in x == (a*(3*a-1))`div`2

-- Number Lists

triangularNumbers :: [Int]
triangularNumbers = scanl1 (+) [1..]

squareNumbers :: [Int]
squareNumbers = scanl1 (+) [1,3..]

pentagonalNumbers :: [Int]
pentagonalNumbers = scanl1 (+) [1,4..]

hexagonalNumbers :: [Int]
hexagonalNumbers = scanl1 (+) [1,5..]

-- List Transformations

-- converts a sorted list to a set
listToSet :: Eq a => [a] -> [a]
listToSet (x:y:xs)
  | x == y    = x:listToSet xs
  | otherwise = x:listToSet (y:xs)
listToSet xs  = xs

-- Combinations of size n where each element is distinct
distinctCombinations :: Int -> [a] -> [[a]]
distinctCombinations 1 xs = fmap (:[]) xs
distinctCombinations n (x:xs) = if n > length xs
  then fmap (x:) (distinctCombinations (n - 1) xs)
  else fmap (x:) (distinctCombinations (n - 1) xs) ++ distinctCombinations n xs

-- Combinations of size n where each element is not necessarily distinct
nonDistinctCombinations :: Int -> [a] -> [[a]]
nonDistinctCombinations n xs = (iterate (\ys -> concatMap (\as -> fmap (:as) xs) ys) $ fmap (\a -> [a]) xs) !! (n-1)

-- Sorting Functions

-- Zip Sort can quickly merge two sorted lists.
zipSort :: Ord a => [a] -> [a] -> [a]
zipSort []     ys = ys
zipSort xs     [] = xs
zipSort (x:xs) (y:ys)
  | x <= y    = x:zipSort  xs   (y:ys)
  | otherwise = y:zipSort (x:xs) ys

zipSortBy :: (a -> a -> Ordering) -> [a] -> [a] -> [a]
zipSortBy _ [] ys = ys
zipSortBy _ xs [] = xs
zipSortBy f (x:xs) (y:ys)
  | f x y /= GT = x:zipSortBy f  xs   (y:ys)
  | otherwise   = y:zipSortBy f (x:xs) ys

-- creates a sorted set
zipSortSet :: Ord a => [a] -> [a] -> [a]
zipSortSet []     ys = ys
zipSortSet xs     [] = xs
zipSortSet (x:xs) (y:ys)
  | x == y    =   zipSortSet (x:xs) ys
  | x < y     = x:zipSortSet  xs   (y:ys)
  | otherwise = y:zipSortSet (x:xs) ys

-- creates the difference xs - ys
zipSortDiff :: Ord a => [a] -> [a] -> [a]
zipSortDiff (x:xs) (y:ys)
  | x == y    =   zipSortDiff  xs    ys
  | x < y     = x:zipSortDiff  xs   (y:ys)
  | otherwise =   zipSortDiff (x:xs) ys
zipSortDiff [] _ = []
zipSortDiff xs _ = xs

zipSortIntersect :: Ord a => [a] -> [a] -> [a]
zipSortIntersect (x:xs) (y:ys)
  | x < y     =   zipSortIntersect  xs   (y:ys)
  | y < x     =   zipSortIntersect (x:xs) ys
  | otherwise = x:zipSortIntersect  xs    ys
zipSortIntersect _ _ = []

zipSortIntersectBy :: (a -> a -> Ordering) -> [a] -> [a] -> [a]
zipSortIntersectBy f (x:xs) (y:ys)
  | f x y == LT =   zipSortIntersectBy f  xs   (y:ys)
  | f x y == GT =   zipSortIntersectBy f (x:xs) ys
  | otherwise   = x:zipSortIntersectBy f  xs    ys
zipSortIntersectBy _ _ _ = []
