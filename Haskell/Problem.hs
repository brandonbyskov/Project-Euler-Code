module Problem where

-- 3 5 1000
problem1 :: Int -> Int -> Int -> Int
problem1 x y max = problem1' x y 0 0 max
	where
		problem1' xi yi x y max
			| (x >= max && y >= max) = 0
			| x == y								 = problem1' xi yi (x + xi) y max
			| x < y									 = x  + problem1' xi yi (x + xi) y max
			| otherwise							 = y  + problem1' xi yi x (y + yi) max