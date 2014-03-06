import System.IO

main = putStrLn . show $ factors 600851475143
	where
		factors x = factors' x 2 1
		factors' 1 _ acc = acc
		factors' x i acc
			| x `mod` i == 0 = factors' (x `div` i) i i
			| otherwise = factors' x (i+1) acc
