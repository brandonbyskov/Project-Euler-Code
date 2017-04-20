module Problem.FileIO where

import System.IO (IOMode(..), hGetContents, openFile)
import Data.Char (digitToInt)

readDigits :: String -> IO [Int]
readDigits path = openFile path ReadMode
                  >>= hGetContents
                  >>= return . fmap digitToInt . filter (/= '\n')

readGrid :: String -> IO [[Int]]
readGrid path = openFile path ReadMode
                >>= hGetContents
                >>= return . fmap (fmap read . words) . lines

readNames :: String -> IO [String]
readNames path = openFile path ReadMode
                 >>= hGetContents
                 >>= return . splitOn ',' . filter (/= '\"')

readIntegerLines :: String -> IO [Integer]
readIntegerLines path = openFile path ReadMode
                    >>= hGetContents
                    >>= return . fmap read . lines

readIntList :: String -> IO [Int]
readIntList path = openFile path ReadMode
               >>= hGetContents
               >>= return . fmap read . splitOn ','

-- File parsing

splitOn :: Char -> String -> [String]
splitOn d s = let (a,s') = break (==d) s
              in if null s'
                   then [a]
                   else a:splitOn d (tail s')
