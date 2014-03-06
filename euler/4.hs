import System.IO
import Data.List

--main = putStrLn . show . head $ factors 600851475143
main = putStrLn . show . head . filter pali $ products
	where
		products = sortBy (flip compare) [x * y | x <- [100..999], y <- [100..x]]
		pali x = let xs = digits x in and $ zipWith (==) xs (reverse xs)
		digits x = digits' x []
		digits' 0 acc = acc
		digits' x acc = digits' (x `div` 10) ((x `mod` 10):acc)
