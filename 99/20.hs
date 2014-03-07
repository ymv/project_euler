import System.Random (randomRIO)

-- Problem 1
last' [] = undefined
last' [x] = x
last' (_:xs) = last' xs

-- Problem 2 
penultimate [] = undefined
penultimate [_] = undefined
penultimate (x:_:[]) = x
penultimate (x:xs) = penultimate xs

-- Problem 3
elementAt [] _ = undefined
elementAt (x:_) 1 = x
elementAt (_:xs) n = elementAt xs (n-1)

-- Problem 4
myLength xs = myLength' xs 0
	where
		myLength' [] acc = acc
		myLength' (_:xs) acc = myLength' xs (acc+1)

-- Problem 5
myReverse xs = myReverse' xs []
	where
		myReverse' [] acc = acc
		myReverse' (x:xs) acc = myReverse' xs (x:acc)

-- Problem 6
isPalindrome xs = xs == (reverse xs)

-- Problem 7
data NestedList a = Elem a | List [NestedList a]

flatten [] = []
flatten ((Elem x):xs) = x:flatten xs
flatten ((List nx):xs) = flatten nx ++ flatten xs

-- Problem 8
compress [] = []
compress (x:xs) = compress' xs x
	where
		compress' [] last = last:[]
		compress' (x:xs) last
			| x == last = compress' xs last
			| otherwise = last:compress' xs x

-- Problem 9
pack [] = pack []
pack (x:xs) = pack' xs x [x]
	where
		pack' [] _ acc = acc:[]
		pack' (x:xs) last acc
			| x == last = pack' xs last (last:acc)
			| otherwise = acc:pack' xs x [x]

-- Problem 10
encode [] = encode []
encode (x:xs) = encode' xs x 1
	where
		encode' [] last n = (n,last):[]
		encode' (x:xs) last n
			| x == last = encode' xs last (n+1)
			| otherwise = (n,last):encode' xs x 1

-- Problem 11
data Encoded a = Multiple Int a | Single a deriving (Show)
encodeModified :: (Eq a) => [a] -> [Encoded a]
encodeModified = map val . encode
	where
		val (1,x) = Single x
		val (n,x) = Multiple n x

-- Problem 12
decodeModified [] = []
decodeModified ((Single x):xs) = x:decodeModified xs
decodeModified ((Multiple n x):xs) = replicate n x ++ decodeModified xs

-- Problem 13
encodeDirect [] = encodeDirect []
encodeDirect (x:xs) = encodeDirect' xs x 1
	where
		encodeDirect' [] last n = (val n last):[]
		encodeDirect' (x:xs) last n
			| x == last = encodeDirect' xs last (n+1)
			| otherwise = (val n last):encodeDirect' xs x 1
		val 1 x = Single x
		val n x = Multiple n x

-- Problem 14
dupli [] = []
dupli (x:xs) = x:x:dupli xs

-- Problem 15
repli [] _ = []
repli (x:xs) n = replicate n x ++ repli xs n

-- Problem 16
dropEvery xs n = map fst . filter snd $ zip xs (map f [1..])
	where f x = (x `mod` n) /= 0

-- Problem 17
split [] _ = ([], [])
split xs 0 = ([], xs)
split (x:xs) n = (x:h, t)
	where (h, t) = split xs (n-1)

-- Problem 18
slice [] _ _ = []
slice _ 1 0 = []
slice (x:xs) 1 m = x:slice xs 1 (m-1)
slice (_:xs) n m = slice xs (n-1) (m-1)

-- Problem 19
rotate xs n | n < 0 = rotate xs (length xs + n)
rotate xs n = rotate' xs n []
	where
		rotate' xs 0 acc = xs ++ reverse acc
		rotate' (x:xs) n acc = rotate' xs (n-1) (x:acc)

-- Problem 20
removeAt _ [] = (undefined, [])
removeAt 1 (x:xs) = (x, xs)
removeAt n (x:xs) = (r, x:t)
	where
		(r,t) = removeAt (n-1) xs

-- Problem 21
insertAt y xs 1 = y:xs
insertAt y (x:xs) n = x:insertAt y xs (n-1)

-- Problem 22
range n m
	| n == m = [n]
	| otherwise = n:range (n+1) m

-- Problem 23
rnd_select xs 0 = return []
rnd_select xs n = rnd >>= return . (flip removeAt) xs >>= moar
	where
		moar (x,tail) = rnd_select tail (n-1) >>= return . (:) x
		rnd = randomRIO (1, length xs)

-- Problem 24
diff_select n max = rnd_select [1..max] n

--Problem 25
rnd_permu xs = rnd_select xs (length xs)

-- Problem 26
combinations n =  harvest . grow_n n . seed
	where
		seed xs = [([], xs)]
		harvest = map fst
		grow_n 0 xs = xs
		grow_n n xs = grow_n (n-1) (grow xs)
		grow xs = concatMap (uncurry iter) xs
		iter p [] = []
		iter p (x:xs) = (x:p, xs):iter p xs
