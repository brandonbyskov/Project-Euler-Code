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
