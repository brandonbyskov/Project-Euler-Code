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
                 >>= return . split . filter (/= '\"')  
  where
    split :: String -> [String]
    split [] = []
    split cs  = case span (/= ',') cs of
    			  (word, [])   -> word:[]
    			  (word, rest) -> word:split (tail rest)

readIntegers :: String -> IO [Integer]
readIntegers path = openFile path ReadMode
                    >>= hGetContents
                    >>= return . fmap read . lines
