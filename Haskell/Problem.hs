module Problem where

import Problem.Support

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
      | y < min = if x == min then highest else problem4' (x-1) (x-1) min highest
      | x * y <= highest   = if x == y then highest else problem4' (x-1) (x-1) min highest
      | isPalindrome (x*y) = problem4' (x-1) (x-1) min (x*y)
      | otherwise = problem4' x (y-1) min highest

-- 20
problem5 :: Int -> Int
problem5 max = problem5' 2 max
  where
    problem5' :: Int -> Int -> Int
    problem5' x max
      | x > max = 1
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
problem7 n = problem7' 0 n
  where
    problem7' :: Int -> Int -> Int
    problem7' prime 0 = prime
    problem7' prime n = problem7' (getNextPrime prime) (n-1)
