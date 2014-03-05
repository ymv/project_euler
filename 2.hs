import System.IO

main = putStrLn . show . sum . filter even . takeWhile (< 4000000) $ fibs
	where
		stream = 1:2:zipWith (+) stream (tail stream)
