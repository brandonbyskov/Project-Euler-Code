module Problem.Support where

import Data.Char (digitToInt)
import Data.List

--Prime functions

primes :: [Int]
primes = 2:[x | x <- tail divisorList, isPrime x]

-- This prime number generator is much faster, but is memory expensive.
primes' :: [Int]
primes' = let ps = drop 3 primes'
          in 2:3:5:7:[x | x <- drop 4 divisorList, not (hasPrimeDivisors x ps)]

hasPrimeDivisors :: Int -> [Int] -> Bool
hasPrimeDivisors x ps = any (\p -> isDivisible p x) . takeWhile (<= sqrRoot x) $ ps

divisorList :: [Int]
divisorList = 2:3:5:(tail $ concatMap (\x -> fmap (x+) primesShortList) [0,30..] )
  where
    primesShortList :: [Int]
    primesShortList = [1,7,11,13,17,19,23,29] 

isPrime :: Int -> Bool
isPrime x
  | x < 5     = x == 2 || x == 3
  | otherwise = not . any (\a -> isDivisible a x) . takeWhile (<= sqrRoot x) $ divisorList

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
isDivisible :: Integral a => a -> a -> Bool
isDivisible d x = (x `mod` d) == 0

numDivisors :: Integral a => a -> a
numDivisors x
  | x <= 1    = if x == 1 then 1 else 0
  | otherwise = 2 + addDivisors 2 (sqrRoot x) x
  where
    addDivisors :: Integral a => a -> a -> a -> a
    addDivisors d root x
      | d >= root       = if d*d == x then 1 else 0
      | isDivisible d x = 2 + addDivisors (d+1) root x
      | otherwise       =     addDivisors (d+1) root x

-- Digit Functions 

digitsToInt :: Integral a => [Int] -> a
digitsToInt = fromIntegral . sum . zipWith (*) (iterate (10*) 1) . reverse

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

-- equivalent to sum [1..n]
sum1ToN :: Integral a => a -> a
sum1ToN n = n*(n+1) `div` 2

sqrRoot :: Integral a => a -> a
sqrRoot = floor . sqrt . fromIntegral

-- Number Tests

isPalindrome :: (Integral a, Show a) => a -> Bool
isPalindrome x 
  | x < 0 = False
  | otherwise = (show x) == reverse (show x)

isPandigital :: Integral a => a -> Bool
isPandigital x = [1..9] == sort (toDigits x)

isTriangular :: Integral a => a -> Bool
isTriangular x = let a = sqrRoot (2*x)
                 in x == (a*a + a) `div` 2

isSquare :: Integral a => a -> Bool
isSquare x = let r = sqrRoot x
             in x == r*r

isPentagonal :: Integral a => a -> Bool
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
