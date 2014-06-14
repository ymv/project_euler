data BFTape a = BFTape [a] a [a]
data BFOp = BFInc | BFDec | BFPInc | BFPDec | BFWhile | BFEnd | BFIn | BFOut
data BFHalt = BFHEndOfROM | BFHStackUnderflow | BFHFFMiss deriving (Show)

class BFSymbol a where
	bfs_default :: a
	bfs_is_zero :: a -> Bool
	bfs_inc :: a -> a
	bfs_dec :: a -> a

instance BFSymbol Int where
	bfs_default = 0
	bfs_is_zero = (== 0)
	bfs_inc = (+ 1)
	bfs_dec = (+ (-1))

tape_new :: BFSymbol a => BFTape a
tape_new = BFTape [] bfs_default []

tape_pinc :: BFSymbol a => BFTape a -> BFTape a
tape_pinc (BFTape as b []) = BFTape (b:as) bfs_default []
tape_pinc (BFTape as b (c:cs)) = BFTape (b:as) c cs

tape_pdec :: BFSymbol a => BFTape a -> BFTape a
tape_pdec (BFTape [] b cs) = BFTape [] bfs_default (b:cs)
tape_pdec (BFTape (a:as) b cs) = BFTape as a (b:cs)

tape_apply :: (a -> a) -> BFTape a -> BFTape a
tape_apply f (BFTape as b cs) = BFTape as (f b) cs

tape_get :: BFTape a -> a
tape_get (BFTape _ x _) = x

tape_set :: a -> BFTape a -> BFTape a
tape_set x (BFTape as _ cs) = BFTape as x cs

execute :: (Enum pc, Monad m, BFSymbol s) => (pc -> Maybe BFOp) -> pc -> (s -> m ()) -> m s -> m BFHalt
execute = execute' tape_new []
	where
		execute' tape stack rom pc io_out io_in =
			case rom pc of
				Nothing -> return BFHEndOfROM
				Just BFInc -> execute'' (tape_apply bfs_inc)
				Just BFDec -> execute'' (tape_apply bfs_dec)
				Just BFPInc -> execute'' tape_pinc
				Just BFPDec -> execute'' tape_pdec
				Just BFOut -> io_out (tape_get tape) >> execute'' id
				Just BFIn -> io_in >>= execute'' . tape_set
				Just BFEnd -> ret
				Just BFWhile
					| bfs_is_zero $ tape_get tape ->
						case fast_forward pc' 1 of
							Nothing -> return BFHFFMiss
							Just jump -> execute' tape stack rom jump io_out io_in
					| otherwise -> call pc'
			where
				execute'' f = execute' (f tape) stack rom pc' io_out io_in
				pc' = succ pc
				fast_forward i 0 = Just i
				fast_forward i depth = case rom i of
					Nothing -> Nothing
					Just BFEnd -> fast_forward (succ i) (depth - 1)
					Just BFWhile -> fast_forward (succ i) (depth + 1)
					Just _ -> fast_forward (succ i) depth
				call jump = execute' tape (pc:stack) rom jump io_out io_in
				ret = case stack of
					[] -> return BFHStackUnderflow
					(jump:stack') -> execute' tape stack' rom jump io_out io_in
string_rom :: [Char] -> Int -> Maybe BFOp
string_rom s pc
	| length s <= pc = Nothing
	| otherwise = decode $ s !! pc
		where
			decode '+' = Just BFInc
			decode '-' = Just BFDec
			decode '>' = Just BFPInc
			decode '<' = Just BFPDec
			decode '[' = Just BFWhile
			decode ']' = Just BFEnd
			decode ',' = Just BFIn
			decode '.' = Just BFOut
			decode _ = Nothing

hello = string_rom "++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++."
ioread :: IO Int
ioread = (getLine >>= return . read)
iowrite x = putStr [s]
	where
		s :: Char
		s = toEnum x
