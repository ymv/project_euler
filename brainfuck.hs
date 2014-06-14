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
execute rom pc io_out io_in = execute' tape_new [] pc (rom, io_out, io_in)
	where
		execute' tape stack pc env@(rom, io_out, io_in) =
			case rom pc of
				Nothing -> return BFHEndOfROM
				Just BFInc -> do_tape_op (tape_apply bfs_inc)
				Just BFDec -> do_tape_op (tape_apply bfs_dec)
				Just BFPInc -> do_tape_op tape_pinc
				Just BFPDec -> do_tape_op tape_pdec
				Just BFOut -> io_out (tape_get tape) >> do_tape_op id
				Just BFIn -> io_in >>= do_tape_op . tape_set
				Just BFEnd -> recall
				Just BFWhile
					| bfs_is_zero $ tape_get tape -> fast_forward
					| otherwise -> mark
			where
				do_tape_op f = execute' (f tape) stack pc' env
				mark = execute' tape (pc:stack) pc' env
				recall = case stack of
					[] -> return BFHStackUnderflow
					(jump:stack') -> execute' tape stack' jump env
				fast_forward = case find_end pc' 1 of
					Nothing -> return BFHFFMiss
					Just jump -> execute' tape stack jump env

				pc' = succ pc
				find_end i 0 = Just i
				find_end i depth = case rom i of
					Nothing -> Nothing
					Just BFEnd -> find_end (succ i) (depth - 1)
					Just BFWhile -> find_end (succ i) (depth + 1)
					Just _ -> find_end (succ i) depth

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
