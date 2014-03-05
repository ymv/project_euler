import System.IO
import Data.List

main = putStrLn . show $ maximized 20
	where
		maximize limit x = limit `div` x * x
		maximized limit = multiply . foldl join_factors [] . map (collate . factors . maximize limit) $ [1..limit]
		
data Factor = Factor Int Int deriving (Show, Eq)

factors x = factors' x 2 []
	where
		factors' 1 _ acc = acc
		factors' x i acc
			| x `mod` i == 0 = factors' (x `div` i) i (i:acc)
			| otherwise = factors' x (i+1) acc

collate [] = []
collate (first:xs) = collate' xs (Factor first 1) []
	where
		collate' [] factor acc = factor:acc
		collate' (x:xs) factor@(Factor x1 n) acc
			| x == x1 = collate' xs (Factor x (n + 1)) acc
			| otherwise = collate' xs (Factor x 1) (factor:acc)

join_factors [] ys = ys
join_factors xs [] = xs
join_factors xs@(fx@(Factor x nx):xst) ys@(fy@(Factor y ny):yst)
	| x == y = (Factor x $ max nx ny):join_factors xst yst
	| x < y = fx:join_factors xst ys
	| otherwise = fy:join_factors xs yst

multiply fs = multiply' fs 1
	where
		multiply' [] acc = acc
		multiply' ((Factor x n):xs) acc = multiply' xs $ acc * (x ^ n)
