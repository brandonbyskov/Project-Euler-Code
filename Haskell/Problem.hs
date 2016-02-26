module Problem where

import Problem.FileIO
import Problem.Support

import Data.Char (digitToInt, intToDigit, ord)
import Data.List
import Data.Maybe (fromJust)
import Data.Ord (comparing)
import Data.Ratio (numerator)
import Numeric (showIntAtBase)

-- 3 5 1000
problem1 :: Int -> Int -> Int -> Int
problem1 a b max = let f x = x * (g $ (max-1) `div` x) -- multiples of x less than max
                       g n = n*(n+1) `div` 2 -- equivalent to sum [1..n]
                   in f a + f b - f (a*b)

-- 4000000
problem2 :: Int -> Int
problem2 max = sum $ takeWhile (< max) evenFibs
  where
    evenFibs :: [Int]  
    evenFibs = 2:8:(zipWith (\a b ->a+4*b) evenFibs (tail evenFibs))

-- 600851475143
problem3 :: Int -> Int
problem3 x = problem3' 2 0
  where
    problem3' :: Int -> Int -> Int
    problem3' divisor highest
      | divisor > root        = highest
      | isDivisible divisor x = if isPrime (x `div` divisor) then (x `div` divisor)
                                else if isPrime divisor then problem3' (divisor+1) divisor
                                     else problem3' (divisor+1) highest
      | otherwise             = problem3' (divisor+1) highest
    root = sqrRoot x

-- 3
problem4 :: Int -> Int
problem4 numDigits = problem4' (10^numDigits - 1) (10^numDigits - 1) 0
  where
    problem4' :: Int -> Int -> Int -> Int
    problem4' x y highest
      | y < min            = if x == min then highest else problem4' (x-1) (x-1) highest
      | x * y <= highest   = if x == y   then highest else problem4' (x-1) (x-1) highest
      | isPalindrome (x*y) = problem4' (x-1) (x-1) (x*y)
      | otherwise          = problem4'  x    (y-1) highest
    min = (10^(numDigits - 1))

-- 20
problem5 :: Int -> Int
problem5 max = problem5' 2
  where
    problem5' :: Int -> Int
    problem5' x = product . fmap largestMultiple . filter isPrime $ [2..max]
    --
    largestMultiple :: Int -> Int
    largestMultiple x = last . takeWhile (< max) $ iterate (*x) x

--100
problem6 :: Int -> Int
problem6 n = squareOfSum [1..n] - sumOfSquares [1..n]
  where
    squareOfSum :: (Integral a) => [a] -> a
    squareOfSum xs = (sum xs)^2
    --
    sumOfSquares :: (Integral a) => [a] -> a
    sumOfSquares xs = sum . fmap (\ x -> x*x) $ xs

-- 10001
problem7 :: Int -> Int
problem7 n = primes'!!(n-1)

-- 13 "data/p008.txt"
problem8 :: Int -> String -> IO Int
problem8 n dataFile = readDigits dataFile
                      >>= return . maximum . fmap problem8' . tails
  where
    problem8' xs = let sublist = take n xs
                   in if length sublist >= n
                        then product sublist
                        else 0

-- 1000
problem9 :: Int -> Int
problem9 n = problem9' 1 (n`div`2 - 1) (n - n`div`2)
  where
    problem9' :: Int -> Int -> Int -> Int
    problem9' a b c
      | b == 1          = 0
      | a^2 + b^2 > c^2 = problem9' a (b-1) (c+1)
      | a^2 + b^2 < c^2 = problem9' (a+1) b (c-1)
      | otherwise       = a*b*c

-- 2000000
problem10 :: Int -> Int
problem10 n = sum $ takeWhile (< n) primes'

-- 500
problem12 :: Int -> Int
problem12 minDivisors = head . filter (\x -> numDivisors x >= minDivisors) $ triangleNumbers
  where
    triangleNumbers = scanl1 (+) [1..]

-- "data/p013.txt"
problem13 :: String -> IO Int
problem13 dataFile = readIntegers dataFile
                     >>= return . fromIntegral . (\a -> a `div` 10^(numDigits a - 10)) . sum

-- 1000000
problem14 :: Int -> Int
problem14 n = fst . maximumBy (comparing snd) . fmap (\i -> (i, collatz i)) $ [1..n-1]
  where
    collatz :: Int -> Int
    collatz 1 = 1
    collatz n
      | even n    = 1 + collatz (n `div` 2)
      | otherwise = 1 + collatz (3*n + 1)

-- 20
problem15 :: Int -> Int
problem15 gridSize = last $ (iterate (buildList 1) []) !! gridSize
  where
    buildList :: Int -> [Int] -> [Int]
    buildList a []     = [2*a]
    buildList a (x:xs) = (a + x):buildList (a + x) xs

-- 1000
problem16 :: Int -> Int
problem16 n
  | n < 0     = 0
  | otherwise = sum (toDigits (2^n))

-- 1000
problem17 :: Int -> Int
problem17 n = sum . fmap letterCount $ [1..n]
  where
    letterCount n
      | n == 1000 = 11
      | n >= 100  = letterCount (n`div`100) + if n`mod`100 == 0 then 7 else (10 + letterCount (n`mod`100))
      | n >= 20   = tensPrefix!!(n`div`10 - 2) + letterCount (n`mod`10)
      | n >= 10   = teensCount!!(n-10)
      | n >= 1    = singleDigitCount!!(n-1)
      | n == 0    = 0
    --number of characters in written number
    tensPrefix = [6,6,5,5,5,7,6,6] --starting with twenty
    teensCount = [3,6,6,8,8,7,7,9,8,8] --starting with ten
    singleDigitCount = [3,3,5,4,4,3,5,5,4] --starting with one

-- "data/p018.txt"
problem18 :: String -> IO Int
problem18 dataFile = readGrid dataFile >>= return . head . foldr1 solveRow
  where
    solveRow :: [Int] -> [Int] -> [Int]
    solveRow []     _          = []
    solveRow (a:as) (b1:b2:bs) = (a + max b1 b2):(solveRow as (b2:bs))

-- 100
problem20 :: Integer -> Int
problem20 x = sum .toDigits . factorial $ x

-- 10000
problem21 :: Int -> Int
problem21 n = sum . filter predicate $ [2..n]
  where
    predicate :: Int -> Bool
    predicate x = x == (sum . properDivisors . sum . properDivisors $ x) && x /= sum (properDivisors x)

-- "data/p022.txt"
problem22 :: String -> IO Int
problem22 dataFile = readNames dataFile
                     >>= return . sum . zipWith (*) [1,2..] . fmap (sum . fmap letterToValue) . sort
  where
    letterToValue c = (ord c) - 64

-- 1000
problem25 :: Int -> Int
problem25 digits = fst . head . dropWhile (\a -> snd a < 10^(digits-1) ) . zip [1..] $ fibs
  where
    fibs :: [Integer]
    fibs = 1:1:zipWith (+) fibs (tail fibs)

-- 10001
problem28 :: Int -> Int
problem28 n
  | n <= 0    = 0
  | even n    = problem28 (n+1) - 3*((n + 1)^2 - n)
  | otherwise = 1 + 10 * sum [2,4..n] + 4 * sum (fmap (\a -> a*a) [1,3..n-2])

-- 100
problem29 :: Integer -> Int
problem29 a = length $ distinctPowers 2
  where
    distinctPowers x
      | x < a  = zipSortSet (fmap (x^) [2..a]) (distinctPowers (x+1))
      | x == a = fmap (x^) [2..a]

-- 5
problem30 :: Int -> Int
problem30 n = sum $ filter (\x -> x == (sum . fmap (^n) . toDigits $ x)) [10..(9^n)*(n+1)]

problem34 :: Int
problem34 = sum . filter predicate $ [10..2540160]
  where
    predicate x = x == (sum . fmap factorial . toDigits $ x)

-- 1000000
problem36 :: Int -> Int
problem36 max = sum . filter isDoubleBasePalindrome $ [1..max-1]
  where
    isDoubleBasePalindrome :: Int -> Bool
    isDoubleBasePalindrome x = isPalindrome x && isBinaryPalindrome x
    --
    isBinaryPalindrome :: Int -> Bool
    isBinaryPalindrome x = (\ list -> list == reverse list) (showIntAtBase 2 intToDigit x "")

problem38 :: Int
problem38 = maximum
            . concatMap (\a -> filter isPandigital . fmap digitsToInt
                             . fmap (concat . fmap toDigits)
                             . dropWhile (\as -> sum (fmap (numDigits) as) <  9)
                             . takeWhile (\as -> sum (fmap (numDigits) as) <= 9)
                             . fmap (fmap (a*)) $ [[1..n] | n <- [2..9]] )
            $ [1..9876]

-- 1000000
problem40 :: Int -> Int
problem40 maxN = problem40' 1 maxN champernowne
  where
    problem40' :: Int -> Int -> [Int] -> Int
    problem40' n maxN list
      | n > maxN  = 1
      | otherwise = head list * problem40' (n*10) maxN (drop (n*9) list)
    champernowne :: [Int]
    champernowne = fmap digitToInt . concat . fmap show $ [1..]

-- 4
problem47 :: Int -> Int
problem47 n = head . head
            . filter ((\xs -> n - 1 + head xs == last xs) . take n)
            . tails
            . filter ((== n) . length . filter isPrime . properDivisors)
            $ [1..]

-- 1000
problem48 :: Int -> Int
problem48 1 = 1
problem48 x = fromIntegral (((toInteger x)^x + (toInteger (problem48 (x-1)))) `mod` 10000000000)

-- 1000000
problem50 :: Int -> Int
problem50 max = let a = length . takeWhile (< max) . scanl1 (+) $ primes' -- length of longest possible chain
                in head . concat . fmap (filter isPrime . takeWhile (< max) . primeSums) $ [a, a-1 .. 1]
  where
    -- sums of n consecutive primes
    primeSums :: Int -> [Int]
    primeSums n = fmap (sum . take n) $ tails primes'

-- 0.1
problem58 :: Double -> Int
problem58 max = problem58' 3 3 0 1 2
  where
    problem58' :: Int -> Int -> Int -> Int-> Int -> Int
    problem58' x 0 numPrimes counted increment = if (realToFrac numPrimes) / (realToFrac (counted+1)) < max
                                                   then increment + 1
                                                   else problem58' (x+increment+2) 3 numPrimes (counted+1) (increment+2)
    problem58' x corner numPrimes counted increment
      | isPrime x = problem58' (x+increment) (corner-1) (numPrimes+1) (counted+1) increment
      | otherwise = problem58' (x+increment) (corner-1)  numPrimes    (counted+1) increment

-- 100
problem65 :: Int -> Int
problem65 maxIter 
  | maxIter <= 0 = 0
  | otherwise    = sum (toDigits (numerator (e 1 maxIter)))
  where
    e :: Int -> Int -> Rational
    e 1 maxIter = 2 + e 2 maxIter
    e iteration maxIter
      | iteration > maxIter     = 0
      | isDivisible 3 iteration = 1 / ((realToFrac (2*iteration `div` 3)) + e (iteration+1) maxIter)
      | otherwise               = 1 / (1 + e (iteration+1) maxIter)

-- "data/p067.txt"
problem67 :: String -> IO Int
problem67 = problem18

problem97 :: Int
problem97 = fromIntegral . (`mod`10000000000) $ 1 + 28433 * 2^7830457

problem104 :: Int
problem104 = 1 + fromJust (elemIndex True (map pandigitalTest fib))
  where
    fib :: [Integer]
    fib = 1:1:(zipWith (+) fib (tail fib))
    pandigitalTest :: Integral a => a -> Bool
    pandigitalTest x = isPandigital (x `mod` 1000000000) && isPandigitalSignificantDigits x
    isPandigitalSignificantDigits :: Integral a => a -> Bool
    isPandigitalSignificantDigits x = [1..9] == (intersect [1..9] (take 9 (toDigits x)))

-- 99 100
problem112 :: Int -> Int -> Int
problem112 x y = fst . head . dropWhile (not . predicate)
                 . iterate (\(a,count) -> let a' = a + 1
                                          in if isBouncy a'
                                               then (a', count + 1)
                                               else (a', count) )
                 $ (100,0)
  where
    isBouncy x = let ds       = toDigits x
                     dsSorted = sort ds
                 in (ds /= dsSorted) && (ds /= reverse dsSorted)
    predicate (total, count) = total`mod`y == 0 && total`div`y == count`div`x

-- 30
problem119 :: Int -> Int
problem119 n = snd . (!! (n - 1)) . filter (\(a,b) -> sum (toDigits b) == a) $ f 7
  where
    f 171 = (171,171*171):g 171 -- highest theoretically with 64-bit Int
    f x   = (x,x*x):zipSortBy (comparing snd) (g x) (f (x+1))
    g x = iterate (\(_,b) -> (x, x * b)) (x,x^3)

--
--problem352 :: Int -> Double
--problem352 sample = sum(fmap (minTests sample) [(fromIntegral p)/100 | p <- [1,2..50]])
--  where
--    minTests sample p =
    --find num tests at various sample splits, get local minimum, then solve with smaller samples recursively

-- 1000000000000000
problem401 :: Int -> Int
problem401 n = problem401' ( n) 1 0 0
  where
    problem401' :: Int -> Int -> Int -> Int -> Int
    problem401' n value lastSS sum
      | n < value =  sum
      | otherwise = do
        let count = n `div` value -- the number of Ints < n that have the same divisors
        let divisor = n `div` count -- the highest divisor that shares the same frequency in the range
        --get the sum of all divisors^2 that share the same frequency of occurence
        let sumSquares' = squarePyramidal divisor
        let sumSquares = if sumSquares'-lastSS < 0 then sumSquares'-lastSS+1000000000 else sumSquares'-lastSS
        problem401' n (divisor+1) sumSquares' (trim (sum+(trim count)*sumSquares))
    -- the following squarePyramidal functions calculate the sum of all the squares from 1^2..n^2.
    -- The complicated pattern matching and modulus trimming are to keep all numbers within the
    -- bounds of the Int type.
    squarePyramidal :: Int -> Int
    squarePyramidal n = squarePyramidal' n (n`mod`6)
    squarePyramidal' :: Int -> Int -> Int
    squarePyramidal' n 0 = squarePyramidal'' (n`div`6)  (n+1)         (2*n+1)
    squarePyramidal' n 5 = squarePyramidal''  n        ((n+1)`div`6)  (2*n+1)
    squarePyramidal' n 4 = squarePyramidal'' (n`div`2)  (n+1)        ((2*n+1)`div`3)
    squarePyramidal' n 3 = squarePyramidal'' (n`div`3) ((n+1)`div`2)  (2*n+1)
    squarePyramidal' n 2 = squarePyramidal'' (n`div`2) ((n+1)`div`3)  (2*n+1)
    squarePyramidal' n 1 = squarePyramidal''  n        ((n+1)`div`2) ((2*n+1)`div`3)
    squarePyramidal'' :: Int -> Int -> Int -> Int
    squarePyramidal'' a b c = trim (trim ((trim a)*(trim b))*(trim c))
    --
    trim :: Int -> Int
    trim n = if n >= 1000000000 then n`mod`1000000000 else n

-- 500500
problem500 :: Int -> Int
problem500 divisors = let trim = \x -> if x >= 500500507 then x `mod` 500500507 else x
                      in foldl' (\a b -> trim (a*b)) 1 . take divisors $ powersOfPrimes 0
  where
    -- These functions create a sorted infinite list of primes and their powers
    powersOfPrimes :: Int -> [Int]
    powersOfPrimes 0 = zipSort primes' (powersOfPrimes 1)
    powersOfPrimes n = (2^2^n):zipSort (fmap (^2^n) $ tail primes') (powersOfPrimes (n+1))

-- Correct but too slow. Needs a better prime number algorithm, 
-- or take advantage of geometric series.
problem518 :: Int -> Int
problem518 n = problem518' (1+head primes') primes' (tail primes') 0
  where
    problem518' :: Int -> [Int] -> [Int] -> Int -> Int
    problem518' n1 pList1 pList2 sum = do
        let n2 = (1 + head pList2)
        let diff' = n2*n2
        let c = diff' `div` n1 - 1
        if ( c < n ) 
          then if (diff' `mod` n1 == 0) && not (hasPrimeDivisors c primes')
                 then
                   problem518' n1 pList1 (tail pList2) (sum + (head pList1) + (head pList2) + c)
                 else problem518' n1 pList1 (tail pList2) sum
          else if head pList1 >= limit
                 then sum
                 else problem518' (1+head (tail pList1)) (tail pList1) (drop 2 pList1) sum
    limit = n - 2 * sqrRoot n
