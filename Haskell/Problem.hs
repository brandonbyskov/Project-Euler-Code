module Problem where

import Problem.Support
import Data.Char
import Data.List
import Data.Ord
import Numeric (showIntAtBase)

-- 3 5 1000
problem1 :: Int -> Int -> Int -> Int
problem1 x y max = problem1' x y 0 0 max
  where
    problem1' xi yi x y max
      | (x >= max && y >= max) = 0
      | x == y                 = problem1' xi yi (x + xi) y max
      | x < y                  = x  + problem1' xi yi (x + xi) y max
      | otherwise              = y  + problem1' xi yi x (y + yi) max

-- 4000000
problem2 :: Int -> Int
problem2 max = problem2' 1 2 max
  where
    problem2' x y max
      | y > max   = 0
      | otherwise = y + problem2' (x + 2*y) (2*x + 3*y) max

-- 600851475143
problem3 :: Int -> Int
problem3 x = problem3' x 2 (floor (sqrt (fromIntegral x))) 0
  where
    problem3' :: Int -> Int -> Int -> Int -> Int
    problem3' x divisor max highest
      | divisor > max         = highest
      | isDivisible divisor x = if isPrime (x `div` divisor) then (x `div` divisor)
                                else if isPrime divisor then problem3' x (divisor+1) max divisor
                                     else problem3' x (divisor+1) max highest
      | otherwise             = problem3' x (divisor+1) max highest

-- 3
problem4 :: Int -> Int
problem4 numDigits = problem4' (10^numDigits - 1) (10^numDigits - 1) (10^(numDigits - 1)) 0
  where
    problem4' :: Int -> Int -> Int -> Int -> Int
    problem4' x y min highest
      | y < min            = if x == min then highest else problem4' (x-1) (x-1) min highest
      | x * y <= highest   = if x == y then highest else problem4' (x-1) (x-1) min highest
      | isPalindrome (x*y) = problem4' (x-1) (x-1) min (x*y)
      | otherwise          = problem4' x (y-1) min highest

-- 20
problem5 :: Int -> Int
problem5 max = problem5' 2 max
  where
    problem5' :: Int -> Int -> Int
    problem5' x max
      | x > max   = 1
      | isPrime x = (multiplyPowers x max x) * problem5' (x+1) max
      | otherwise = problem5' (x+1) max
    --
    multiplyPowers :: Int -> Int -> Int -> Int
    multiplyPowers x max total
      | total * x > max = total
      | otherwise       = multiplyPowers x max (total*x)

problem6 :: Int -> Int
problem6 max = squareOfSum [1..max] - sumOfSquares [1..max]
  where
    squareOfSum :: (Integral a) => [a] -> a
    squareOfSum list = (sum list)^2
    --
    sumOfSquares :: (Integral a) => [a] -> a
    sumOfSquares list = sum (fmap (\ x -> x*x) list)

-- 10001
problem7 :: Int -> Int
problem7 n = primes'!!(n-1)

-- 2000000
problem10 :: Int -> Int
problem10 max = problem10' max primes'
  where
    problem10' max pList
      | head pList >= max = 0
      | otherwise         = head pList + problem10' max (tail pList)

-- 500
problem12 :: Int -> Int
problem12 minDivisors = head [x::Int | x <- triangleNumbers, (numDivisors x) >= minDivisors]
  where
    triangleNumbers :: [Int]
    triangleNumbers = scanl1 (+) [1..]


-- I'd ideally like to implement memoization for the collatz function.
-- 
-- 1000000
problem14 :: Int -> Int
problem14 n = fst (maximumBy (comparing snd) [(i, collatz i)::(Int,Int) | i <- [1..(n-1)]])
  where
    collatz :: Int -> Int
    collatz 1 = 1
    collatz n
      | even n    = 1 + collatz (n `div` 2)
      | otherwise = 1 + collatz (3*n + 1)

-- 1000
problem16 :: Int -> Int
problem16 n
  | n < 0     = 0
  | otherwise = sum (fmap digitToInt (show (2^n)))

-- 100
problem20 :: Int -> Int
problem20 x = sum (fmap digitToInt (show (factorial x)))
  where
    factorial ::  (Integral a) => a -> Integer
    factorial n = foldl1 (*) [1..(toInteger n)]

-- 1000000
problem36 :: Int -> Int
problem36 max = sum [x::Int | x <- [1..(max-1)], isDoubleBasePalindrome x]
  where
    isDoubleBasePalindrome :: Int -> Bool
    isDoubleBasePalindrome x = isPalindrome x && isBinaryPalindrome x
    --
    isBinaryPalindrome :: Int -> Bool
    isBinaryPalindrome x = (\ list -> list == reverse list) (showIntAtBase 2 intToDigit x "")

-- 1000000
problem40 :: Int -> Int
problem40 maxN = problem40' 1 maxN champernowne
  where
    problem40' :: Int -> Int -> [Int] -> Int
    problem40' n maxN list
      | n > maxN  = 1
      | otherwise = head list * problem40' (n*10) maxN (drop (n*9) list)
    champernowne :: [Int]
    champernowne = fmap digitToInt (concat (fmap show [1..]))

-- 1000000
problem50 :: Int -> Int
problem50 max = problem50' 2 max primes' 2
  where
    problem50':: Int -> Int -> [Int] -> Int -> Int
    problem50' n max pList highest
      | sum (take n pList) >= max    = if head pList == 2 then highest
                                       else problem50' (n+1) max primes' highest
      | isPrime (sum (take n pList)) = problem50' (n+1) max primes' (sum (take n pList))
      | otherwise                    = problem50' n max (tail pList) highest

-- 0.1
problem58 :: Double -> Int
problem58 max = problem58' max 3 3 0 1 2
  where
    problem58' :: Double -> Int -> Int -> Int -> Int-> Int -> Int
    problem58' max x 0 numPrimes counted increment = if (realToFrac numPrimes) / (realToFrac (counted+1)) < max then increment + 1
                                                     else problem58' max (x+increment+2) 3 numPrimes (counted+1) (increment+2)
    problem58' max x corner numPrimes counted increment
      | isPrime x = problem58' max (x+increment) (corner-1) (numPrimes+1) (counted+1) increment
      | otherwise = problem58' max (x+increment) (corner-1)  numPrimes    (counted+1) increment