import System.IO
import Data.List

--main = sequence_ . map (putStrLn . show) $ maximized 20
main = putStrLn . show . delta $ 100
	where
		sum_squared n = square $ n * (n+1) `div` 2
		square x = x*x
		sum_squares n = sum . map square $ [1..n]
		delta x = sum_squared x - sum_squares x
