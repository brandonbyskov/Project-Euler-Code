module Problem.FileIO where

import System.IO

readGrid :: String -> IO [[Int]]
readGrid path = openFile path ReadMode
                >>= hGetContents
                >>= return . fmap (fmap read . words) . lines