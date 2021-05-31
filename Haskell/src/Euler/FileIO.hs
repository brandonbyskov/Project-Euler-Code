module Euler.FileIO where

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

readStrings :: String -> IO [String]
readStrings path = openFile path ReadMode
                 >>= hGetContents
                 >>= return . splitOn ',' . filter (/= '\"')

readStringLines :: String -> IO [String]
readStringLines path = openFile path ReadMode
                   >>= hGetContents
                   >>= return . lines

readIntegerLines :: String -> IO [Integer]
readIntegerLines path = openFile path ReadMode
                    >>= hGetContents
                    >>= return . fmap read . lines

readIntList :: String -> IO [Int]
readIntList path = openFile path ReadMode
               >>= hGetContents
               >>= return . fmap read . splitOn ','

readIntPairLines :: String -> IO [(Int,Int)]
readIntPairLines path = openFile path ReadMode
                    >>= hGetContents
                    >>= return
                      . fmap ((\[a,b] -> (a,b)) . fmap read . (splitOn ','))
                      . lines

-- File parsing

splitOn :: Char -> String -> [String]
splitOn d s = let (a,s') = break (==d) s
              in if null s'
                   then [a]
                   else a:splitOn d (tail s')
