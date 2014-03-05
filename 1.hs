import System.IO

main = putStrLn . show . sum . multiplies $ [1..999]
	where
		multiplies = filter $ yoba_combinator divisable (||) 3 5
		divisable x = (== 0) . (`mod` x)
		yoba_combinator f g x y z = g (f x z) (f y z)
