module Euler.Problem where

import Euler.FileIO
import Euler.Support

import Data.Bits (xor)
import Data.Char (digitToInt, intToDigit, ord)
import Data.List
import Data.Maybe (fromJust)
import Data.Ord (comparing)
import Data.Ratio ((%), numerator, denominator)
import Numeric (showIntAtBase)

-- 3 5 1000
problem1 :: Int -> Int -> Int -> Int
problem1 a b max = let f x = x * (sum1ToN $ (max-1) `div` x) -- multiples of x less than max
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
problem6 n = (sum1ToN n)^2 - (sum . take n $ squareNumbers)

-- 10001
problem7 :: Int -> Int
problem7 n = primes!!(n-1)

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
problem10 n = sum $ takeWhile (< n) primes

-- "data/p011.txt"
problem11 :: String -> IO Int
problem11 dataFile = readGrid dataFile
                 >>= return . maximum . (\as->  fmap (maximum . ($as)) [ maxHorizontal
                                                                       , maxVertical
                                                                       , maxForwardDiagonal
                                                                       , maxBackDiagonal
                                                                       ] )
  where
    maxHorizontal      = fmap (maximum . (\xs -> zipWith4 (\a b c d -> a*b*c*d) xs (tail xs) (drop 2 xs) (drop 3 xs) ))
    maxVertical        = fmap (\xss -> case xss of
                                         (as:bs:cs:ds:_) -> maximum $ zipWith4 (\a b c d -> a*b*c*d) as bs cs ds
                                         _               -> 0
                            ) . tails
    maxForwardDiagonal = fmap (\xss -> case xss of
                                         (as:bs:cs:ds:_) -> maximum $ zipWith4 (\a b c d -> a*b*c*d) (drop 3 as) (drop 2 bs) (tail cs) ds
                                         _               -> 0
                              ) . tails
    maxBackDiagonal    = fmap (\xss -> case xss of
                                         (as:bs:cs:ds:_) -> maximum $ zipWith4 (\a b c d -> a*b*c*d) as (tail bs) (drop 2 cs) (drop 3 ds)
                                         _               -> 0
                              ) . tails

-- 500
problem12 :: Int -> Int
problem12 minDivisors = head . filter (\x -> numDivisors x >= minDivisors) $ triangularNumbers

-- "data/p013.txt"
problem13 :: String -> IO Int
problem13 dataFile = readIntegerLines dataFile
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
problem22 dataFile = readStrings dataFile
                 >>= return . sum . zipWith (*) [1,2..] . fmap (sum . fmap letterToValue) . sort
  where
    letterToValue c = (ord c) - 64

problem23 :: Int
problem23 = sum1ToN 28123 - sum (takeWhile (<= 28123) abundantSums)
  where
    abundantSums = foldr1 (\ (x:xs) (ys) -> (x+x):zipSortSet (fmap (x+) (xs)) ys) (tails abundantNumbers)
    abundantNumbers = filter (\x -> x < sum (properDivisors x)) [12..]

-- 1000000
problem24 :: Int -> String
problem24 n = fmap intToDigit $ problem24 (n-1) ds
  where
    ds = [0..9]
    problem24 0 xs = xs
    problem24 n xs = let len     = length xs
                         (d, n') = n `divMod` factorial (len-1)
                         digit   = xs!!d
                     in digit:problem24 n' (delete digit xs)

-- 1000
problem25 :: Int -> Int
problem25 digits = fst . head . dropWhile (\a -> snd a < 10^(digits-1) ) . zip [1..] $ fibs
  where
    fibs :: [Integer]
    fibs = 1:1:zipWith (+) fibs (tail fibs)

-- 1000
problem26 :: Int -> Int
problem26 n = fst . maximumBy (comparing snd) . fmap recipCycle $ [1..n-1]
  where
    recipCycle :: Int -> (Int,Int)
    recipCycle a = let (x:xs) = tail $ decimalExpansion a
                   in (a, loopSize [(x,1)] xs 2)
    decimalExpansion :: Int -> [(Int,Int)]
    decimalExpansion denom = iterate (\(a,r) -> divMod (10*r) denom) (0,1)
    loopSize :: [((Int,Int),Int)] -> [(Int,Int)] -> Int -> Int
    loopSize as (b:bs) depth = let matches = filter (\(x,y) -> x == b) as
                               in if matches == []
                                    then loopSize ((b,depth):as) (tail bs) (depth + 1)
                                    else depth - snd (head matches)

-- 1000
problem27 :: Int -> Int
problem27 n = let bs = takeWhile (< n) primes
                  a' = if even n then n - 1 else n - 2
                  as = [negate a',(negate a' + 2)..a']
              in (\(a,b,c) -> a*b ) . maximumBy (comparing (\(a,b,c) -> c)) $ [(a,b,consecutivePrimes a b) | a <- as, b <- bs]
  where
    consecutivePrimes a b = length . takeWhile (\x -> isPrime (x*x + a*x + b) ) $ [0..]

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

-- 200
problem31 :: Int -> Int
problem31 total = sum $ map (\a -> f a coins total) [0..total `div` head coins]
  where
    coins = reverse [1,2,5,10,20,50,100,200]
    f n [x] remaining
      | n * x == remaining = 1
      | otherwise = 0
    f n (x:xs) remaining
      | n * x == remaining = 1
      | n * x >  remaining = 0
      | otherwise = let remaining' = remaining - n * x
                    in sum $ map (\a -> f a xs remaining') [0..remaining' `div` head xs]

problem32 :: Int
problem32 = sum . listToSet . sort . concatMap f $ permutations [1..9]
  where
    f :: [Int] -> [Int]
    f xs = let (ys, c') = splitAt 5 xs
               (a1, b1) = splitAt 1 ys
               (a2, b2) = splitAt 2 ys
               c        = digitsToInt c'
           in if test (head a1) (digitsToInt b1) c || test (digitsToInt a2) (digitsToInt b2) c
                then [c]
                else []
    test :: Int -> Int -> Int -> Bool
    test a b c = a * b == c

problem33 :: Int
problem33 = (\(a, b) -> denominator (a%b)) . foldr1 (\(a, b) (a', b') -> (a*a', b*b')) $ filter p cases
  where
    cases = filter (\(a,b) -> a < b) $ concatMap (\a -> fmap (\b-> (a, b)) [1..9]) [1..9]
    p (a, b) = any (\(x, y) -> x*b == y*a) . fmap (\n-> (10*a + n, 10*n + b)) $ dropWhile (<= a) [1..9]

problem34 :: Int
problem34 = sum . filter predicate $ [10..2540160]
  where
    predicate x = x == (sum . fmap factorial . toDigits $ x)

-- 1000000
problem35 :: Int -> Int
problem35 limit = length . filter isCircularPrime $ takeWhile (< limit) primes
  where
    isCircularPrime = all isPrime . rotations
    rotations x = map (\n -> digitsToInt . (\(as,bs) -> bs ++ as) . splitAt n $ toDigits x) [1..numDigits x - 1]

-- 1000000
problem36 :: Int -> Int
problem36 max = sum . filter isDoubleBasePalindrome $ [1..max-1]
  where
    isDoubleBasePalindrome :: Int -> Bool
    isDoubleBasePalindrome x = isPalindrome x && isBinaryPalindrome x
    --
    isBinaryPalindrome :: Int -> Bool
    isBinaryPalindrome x = (\ list -> list == reverse list) (showIntAtBase 2 intToDigit x "")

problem37 :: Int
problem37 = sum . filter isTruncatableLtR . concatMap buildTruncatableRtL $ baseDigits
  where
    baseDigits = [2,3,5,7]
    ds = [1,3,7,9]
    isTruncatableLtR x = all isPrime . fmap (x`mod`) . takeWhile (<x) $ iterate (*10) 10
    buildTruncatableRtL x
      | x < 10    =   concatMap (buildTruncatableRtL . (10*x +)) ds
      | isPrime x = x:concatMap (buildTruncatableRtL . (10*x +)) ds
      | otherwise = []

problem38 :: Int
problem38 = maximum
          . concatMap (\a -> filter isPandigital . fmap digitsToInt
                             . fmap (concat . fmap toDigits)
                             . dropWhile (\as -> sum (fmap (numDigits) as) <  9)
                             . takeWhile (\as -> sum (fmap (numDigits) as) <= 9)
                             . fmap (fmap (a*)) $ [[1..n] | n <- [2..9]] )
          $ [1..9876]

-- 1000
problem39 :: Int -> Int
problem39 n = fst . maximumBy (comparing snd) . fmap (\a -> (a, rightAngleSolutions a)) $ [3..n]
  where
    rightAngleSolutions x = length . filter isPythagoreanTriple $ [(a,b, x-(a+b)) | b <- [1..x`div`2], a <- [1..b], a+b <= 2*x`div`3]
    isPythagoreanTriple (a,b,c) = a*a + b*b == c*c

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

problem41 :: Int
problem41 = maximum . filter isPrime $ pandigitals
  where
    pandigitals :: [Int]
    pandigitals = fmap (sum . zipWith (*) (iterate (10*) 1)) $ concatMap (\n -> permutations [1..n]) [1..9]

-- "data/p042.txt"
problem42 :: String -> IO Int
problem42 dataFile = readStrings dataFile
                 >>= return . length . filter isTriangular . fmap sum . (fmap . fmap) letterToValue
  where
    letterToValue :: Char -> Int
    letterToValue c = (ord c) - 64

problem43 :: Int
problem43 = sum . map digitsToInt
          . filter (and . zipWith isDivisible [2,3,5,7,11,13,17] . map (digitsToInt . take 3) . tail . tails)
          . filter (\xs -> head xs /= 0)
          $ permutations [0..9]

problem44 :: Int
problem44 = problem44' pentagonalNumbers (tail pentagonalNumbers) (maxBound :: Int)
  where
    problem44' ps1 (p2:ps2) minD
      = let ps1' = dropWhile (\a -> p2 - a >= minD) ps1
            ds   = fmap (p2-) . filter (\a -> isPentagonal (p2+a) && isPentagonal (p2-a)) $ takeWhile (< p2) ps1'
        in if head ps1' == p2
             then minD
             else problem44' ps1' ps2 (minimum $ minD:ds)

-- 40755
problem45 :: Int -> Int
problem45 n = head . dropWhile (<=n)
            $ triangularNumbers `zipSortIntersect` (pentagonalNumbers `zipSortIntersect` hexagonalNumbers)

problem46 :: Int
problem46 = head . filter (not . f) $ zipSortDiff [9,11..] primes
  where
    f x = any (\p -> isSquare ((x-p)`div`2)) . takeWhile (<=x-2) $ tail primes

-- 4
problem47 :: Int -> Int
problem47 n = head . head
            . filter ((\xs -> n - 1 + head xs == last xs) . take n)
            . tails
            . filter ((== n) . length . filter isPrime . properDivisors)
            $ [1..]

-- 1000
problem48 :: Int -> Int
problem48 x = fromIntegral . (`mod` 10000000000) . sum . fmap (\a -> a^a ) $ [1..toInteger x]

problem49 :: Int
problem49 = problem49' ps (tail ps) (drop 2 ps)
  where
    problem49' (p1:ps1) (p2:ps2) (p3:ps3)
      | p2 - p1 < p3 - p2 = problem49' (p1:ps1)     ps2  (p3:ps3)
      | p2 - p1 > p3 - p2 = problem49' (p1:ps1) (p2:ps2)     ps3
      | isPermutation p1 p2 && isPermutation p1 p3 && (p1,p2) /= (1487,4817) = 100000000*p1 + 10000*p2 + p3
      | otherwise         = problem49' (p1:ps1)     ps2      ps3
    problem49' (_:ps1) _ _ = problem49' ps1 (drop 1 ps1) (drop 2 ps1)
    ps = dropWhile (<=1000) $ takeWhile (<10000) primes
    isPermutation a b = (sort $ toDigits a) == (sort $ toDigits b)

-- 1000000
problem50 :: Int -> Int
problem50 max = let a = length . takeWhile (< max) . scanl1 (+) $ primes -- length of longest possible chain
                in head . concat . fmap (filter isPrime . takeWhile (< max) . primeSums) $ [a, a-1 .. 1]
  where
    -- sums of n consecutive primes
    primeSums :: Int -> [Int]
    primeSums n = fmap (sum . take n) $ tails primes

-- 6
problem52 :: Int -> Int
problem52 n = let xs = concatMap (\a -> [a`div`10..a`div`n]) (iterate (*10) 100)
              in head $ filter sameDigitMultiples xs
  where
    sameDigitMultiples :: Int -> Bool
    sameDigitMultiples x = let dss = fmap (sort . toDigits . (x*)) [1..n]
                           in replicate n (head dss) == dss

-- 100 1000000
problem53 :: Int -> Int -> Int
problem53 maxN minC = length . filter (> toInteger minC) $ [combinations n r | n <- [1..maxN], r <- [1..n]]
  where
    fact :: Int -> Integer
    fact = (factorials!!)
    factorials :: [Integer]
    factorials = 1:zipWith (*) [1..] factorials
    combinations n r = (fact n) `div` (fact r * fact (n-r))

-- 10000 50
problem55 :: Int -> Int -> Int
problem55 n limit = length . filter isLychrel $ [1..(toInteger n)]
  where
    isLychrel = not . any isPalindrome . take limit . tail . iterate (\a -> a + reverseDigits a)

-- 100
problem56 :: Int -> Int
problem56 n = maximum . fmap (sum . toDigits) $ [a ^ b| a <- [1..(fromIntegral n)], b <- [1..n]]

-- 1000
problem57 :: Int -> Int
problem57 n = length . filter (\a -> (numDigits . numerator $ a) > (numDigits . denominator $ a) )
            . take n $ convergents
  where
    convergents :: [Rational]
    convergents = fmap ($ 1%2) . iterate (\a b-> a $ 1/(2+b) ) $ (1+)

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

-- "data/p059.txt" " the "
problem59 :: String -> String -> IO Int
problem59 dataFile searchString = readIntList dataFile
                              >>= (\m -> return . sum . head . filter (isInfixOf str) $ fmap (decrypt m) keys)
  where
    decrypt :: [Int] -> [Int] -> [Int]
    decrypt message key = zipWith xor message (cycle key)
    keys :: [[Int]]
    keys = nonDistinctCombinations 3 [97..122]
    str = fmap ord searchString

problem61 :: Int
problem61 = sum . fmap extract . head
          . filter (\xs -> length xs == 6) . fmap (nubBy (\(a,_,_,_) (b,_,_,_) -> a==b) ) . filter (isCycle)
          . concatMap (\a-> fmap (a:) (match 5 a) ) $ triNums
  where
    toDetails (n, x) = let (f,l) = x `divMod` 100 in (n,x,f,l)
    numberLists = fmap (dropWhile (<1000) . takeWhile (<10000))
                $ [ squareNumbers
                  , pentagonalNumbers
                  , hexagonalNumbers
                  , scanl1 (+) [1,6..]
                  , scanl1 (+) [1,7..]
                  ]
    extract (_,a,_,_) = a
    sortedDetails = concat
                  . (fmap . fmap) toDetails . zipWith (\a bs -> fmap (\b-> (a,b)) bs) [(4::Int)..]
                  $ numberLists
    triNums       = fmap toDetails . fmap (\b-> (3,b)) . dropWhile (<1000) . takeWhile (<10000) $ triangularNumbers
    match 1 (_,_,_,l) = let matches = filter (\(_,_,f,_) -> f == l) sortedDetails
                        in fmap (\a -> [a]) matches
    match n (_,_,_,l) = let matches = filter (\(_,_,f,_) -> f == l) sortedDetails
                        in concatMap (\a -> fmap (a:) (match (n-1) a) ) matches
    isCycle xs = let (_,_,_,l) = last xs
                     (_,_,f,_) = head xs
                 in f == l

-- 5
problem62 :: Int -> Int
problem62 n = fst . head
            . head . dropWhile (\as -> (length as) < n) . concat
            . fmap (groupBy (\(_,a) (_,b) -> a == b))
            . fmap (\as -> sortBy (comparing (digitsToInt . snd)) as)
            $ (fmap . fmap) (\a -> (a, (sort $ toDigits a))) segmentedCubes
  where
    cubes = fmap (^3) [1..]
    segmentedCubes = segment (fmap (\a -> (<a)) $ iterate (*10) 10) cubes
    segment :: [a -> Bool] -> [a] -> [[a]]
    segment (p:ps) as = let (as', bs') = span p as
                        in as':segment ps bs'

problem63 :: Int
problem63 = sum
          . fmap (length . takeWhile (== True) . zipWith (==) [1..] . fmap numDigits . (\a -> iterate (*a) a))
          $ [1..9]

-- 100
problem65 :: Int -> Int
problem65 maxIter 
  | maxIter <= 0 = 0
  | otherwise    = sum . toDigits . numerator $ e 1 maxIter
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

-- 1000000
problem69 :: Int -> Int
problem69 limit = last . takeWhile (<= limit) $ scanl1 (*) primes

-- 100
problem76 :: Int -> Int
problem76 n = (last xs) - 1
  where
    xs = 1:1:buildXs 2 (repeat 1)
    buildXs i as
      | i > n = []
      | otherwise = let bs = (take i xs) ++ (a':as')
                        (a':as') = zipWith (+) as bs
                    in a':buildXs (i+1) as'

-- 10000000
problem92 :: Int -> Int
problem92 limit = length . filter (==89) . fmap digitChain $ [1..(limit-1)]
  where
    digitChain 1  = 1
    digitChain 89 = 89
    digitChain x  = digitChain . sum . fmap (^2) . toDigits $ x

-- 1000000000
problem94 :: Int -> Int
problem94 limit = sum $ takeWhile (<= limit) perimeters
  where
    perimeters = 16:50:zipWith (+) (cycle [12, -12]) (zipWith (\a b -> 4*a - b) (tail perimeters) perimeters)

problem97 :: Int
problem97 = fromIntegral . (`mod`10000000000) $ 1 + 28433 * 2^7830457

-- 1000000000000
problem100 :: Int -> Int
problem100 n = fst . head . dropWhile ((<= n) . snd)
             . fmap (\(a, b) -> ((a`div`2) + 1, (b`div`2) + 1))
             . filter (\(a, b) -> even (a + b)) $ continuedFracs
  where
    continuedFracs :: [(Int,Int)]
    continuedFracs = fmap (\(num, den) -> (den, num+den)) $ iterate (\(num, den) -> (den, 2 * den + num)) (12, 29)

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

-- 10000000000
problem123 :: Int -> Int
problem123 limit = fromIntegral . fst . head . dropWhile (\(n,r) -> r <= fromIntegral limit)
                 . fmap remainder . dropWhile (\(n,p) -> p < (fromIntegral . sqrRoot $ limit))
                 $ zip [1..] (fmap fromIntegral primes)
  where
    remainder :: (Integer,Integer) -> (Integer,Integer)
    remainder (n,p) = let pSqr = p*p
                          r    = (`mod`pSqr) $ (powerMod (p-1) n pSqr) + (powerMod (p+1) n pSqr)
                      in (n, r)

-- 100000000
problem125 :: Int -> Int
problem125 n = sum . foldr1 zipSortSet
             . fmap (filter isPalindrome)
             . takeWhile (not . null)
             . fmap (takeWhile (<n) . tail . scanl1 (+))
             $ tails squareNumbers

-- 1000
problem150 :: Int -> Int
problem150 n = let pyramid = (reverse . take n $ buildPyramid 1 $ generator 0)
               in problem150' (tail pyramid) (fmap (:[]) . head $ pyramid) (replicate (n-1) []) (maxBound :: Int)
  where
    problem150' :: [[Int]] -> [[Int]] -> [[Int]] -> Int -> Int
    problem150'  []      _    _     minVal = minVal
    problem150' (xs:xss) subs diffs minVal = let subs'   = mkSubTriangle xs subs diffs
                                                 minVal' = minimum . (minVal:) . fmap minimum $ subs'
                                             in problem150' xss (zipWith (:) xs subs') (init . tail $ subs) $! minVal'
    mkSubTriangle [] _ _ = []
    mkSubTriangle (x:xs) (ys1:ys2:yss) (ds:dss) = (zipWith3 (\y1 y2 d -> x + y1 + y2 - d) ys1 ys2 (0:ds)):mkSubTriangle xs (ys2:yss) dss
    generator :: Int -> [Int]
    generator t = let t' = (615949*t + 797807) `mod` 1048576
                  in (t' - 524288):generator t'
    buildPyramid n xs = let (ys, zs) = splitAt n xs
                        in ys:buildPyramid (n+1) zs

-- "UDDDUdddDDUDDddDdDddDDUDDdUUDd" 1000000000000000
problem277 :: String -> Int -> Int
problem277 str n = let (x, d1) = findMin str (zip [1..] [1..]) (1,1)
                   in head . filter (>n) $ iterate (+d1) x
  where
    findMin :: String -> [(Int, Int)] -> (Int, Int) -> (Int,Int)
    findMin [] ((x,_):_) (d1,_)  = (x, d1)
    findMin ('D':cs) xys (d1,d2) = let d1'  = 3*d1
                                       xys' = iterate (\(x,y) -> (x+d1', y+d2 )) . (\(x,y) -> (x,y`div`3))         . head . filter (\(_,y) -> y`mod`3 == 0 ) $ xys
                                   in findMin cs xys' (d1', d2 )
    findMin ('U':cs) xys (d1,d2) = let d1'  = 3*d1
                                       d2'  = 4*d2
                                       xys' = iterate (\(x,y) -> (x+d1', y+d2')) . (\(x,y) -> (x,(4*y + 2)`div`3)) . head . filter (\(_,y) -> y`mod`3 == 1 ) $ xys
                                   in findMin cs xys' (d1', d2')
    findMin ('d':cs) xys (d1,d2) = let d1'  = 3*d1
                                       d2'  = 2*d2
                                       xys' = iterate (\(x,y) -> (x+d1', y+d2')) . (\(x,y) -> (x,(2*y - 1)`div`3)) . head . filter (\(_,y) -> y`mod`3 == 2 ) $ xys
                                   in findMin cs xys' (d1', d2')

-- 32
problem323 :: Int -> Double
problem323 bits = expectedValues!!(bits-1)
  where
    expectedValues :: [Double]
    expectedValues = fmap expected [1..]
    expected z = let total = 2^z
                     x = sum . zipWith (*) (fmap fromIntegral . init $ triangle z) $ 1:fmap (+1) expectedValues
                     y = (/total) . foldr1 (\a b -> a+(b/total)) $ take (64`div`z) [total-1+x, 2*(total-1)+x..]
                 in (x + y)/total
    triangle :: Int -> [Int]
    triangle 1 = [1,1]
    triangle n = let tn1 = triangle $ n-1
                 in 1:(zipWith (+) (tail tn1) tn1) ++ [1]

-- (21^7) (7^21) (12^7)
problem340 :: Int -> Int -> Int -> Int
problem340 a b c = let fb        = f b
                       fbMinusa  = f (b-a)
                       delta     = trim $ fbMinusa - fb
                       xs        = take (b`div`a) $ iterate (\x -> trim $ x + delta) (trim fb)
                       lastX     = trim $ (trim fb) + trim ((trim (b`div`a))*delta)
                       remaining = (b+1)`mod`a
                       remainder = if (remaining > 0) then trim $ lastX * remaining - (sum1ToN $ remaining - 1) else 0
                       parts     = fmap (\x -> trim $ x*(trim a) - (trim $ sum1ToN (a-1))) $ xs
                   in foldl1' (\x y -> trim $ x+y) (remainder:parts)
  where
    f n
      | n > b     = n - c
      | otherwise = fStack 4 (n+ a)
    fStack t n
      | t == 0    = n - a
      | n > b     = fStack (t - 1) (a + n - c)
      | otherwise = fStack (t + 3) (a + n)
    trim x = x`mod`1000000000

--
--problem352 :: Int -> Double
--problem352 sample = sum(fmap (minTests sample) [(fromIntegral p)/100 | p <- [1,2..50]])
--  where
--    minTests sample p =
    --find num tests at various sample splits, get local minimum, then solve with smaller samples recursively

-- 1000000000000000
problem401 :: Int -> Int
problem401 n = problem401' n 1 0 0
  where
    problem401' :: Int -> Int -> Int -> Int -> Int
    problem401' n value lastSS sum
      | n < value =  sum
      | otherwise = do
        let count = n `div` value   -- the number of Ints < n that have the same divisors
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
    powersOfPrimes 0 = zipSort primes (powersOfPrimes 1)
    powersOfPrimes n = (2^2^n):zipSort (fmap (^2^n) $ tail primes) (powersOfPrimes (n+1))

-- Correct but too slow. Needs a better prime number algorithm, 
-- or take advantage of geometric series.
problem518 :: Int -> Int
problem518 n = sum . fmap sum
             $ zipWith partialSum (takeWhile (<= limit) ns) (tail $ tails ns)
  where
    partialSum :: Int -> [Int] -> [Int]
    partialSum n1 (n2:ns2) =
        let (n3,r) = (n2*n2) `divMod` n1
        in if n3 <= n
           then if r == 0 && isPrime (n3 - 1)
                  then (n1 + n2 + n3 - 3):partialSum n1 ns2
                  else partialSum n1 ns2
           else []
    limit = n - 2 * sqrRoot n
    ns = fmap (+1) primes
