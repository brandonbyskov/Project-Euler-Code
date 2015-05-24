module Problem.Support where

isPrime :: (Integral a) => a -> Bool
isPrime x
  | x < 5           = if x == 2 || x == 3 then True else False
  | even x          = False
  | isDivisible 3 x = False
  | otherwise       = not (hasDivisors 5 (floor (sqrt (fromIntegral x))) x)
    where
      hasDivisors :: (Integral a) => a -> a -> a -> Bool
      hasDivisors divisor max x
        | divisor > max = False
        | otherwise     = isDivisible divisor x || isDivisible (divisor + 2) x || hasDivisors (divisor+6) max x

getNextPrime :: (Integral a) => a -> a
getNextPrime x
  | x < 2     = 2
  | even x    = if isPrime (x+1) then x+1 else getNextPrime (x+1)
  | otherwise = if isPrime (x+2) then x+2 else getNextPrime (x+2)

-- True if x is divisible by d
isDivisible :: (Integral a) => a -> a -> Bool
isDivisible d x = (x `mod` d) == 0

isPalindrome :: (Integral a, Show a) => a -> Bool
isPalindrome x 
  | x < 0 = False
  | otherwise = (show x) == reverse (show x)
