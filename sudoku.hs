import Data.Char
import Data.Bits
import Data.List
import Data.List.Split

import Control.Monad
import System.Environment
import Control.Parallel.Strategies

import Text.Read
import Text.ParserCombinators.ReadP
import Text.ParserCombinators.ReadPrec


-- | Map arguments to readFile, solve and print
main :: IO ()
main = getArgs
  >>= (mapM process . zip [1..]
       >=> return . intercalate "\n\n\n"
       >=> putStrLn)

process :: (Int,String) -> IO String
process (i,p) = do
  f <- readFile p
  let (n,s) = solve $ read f
  return $ "Sudoku " ++ show i ++ ": "
    ++ check s ++ " in " ++ show n ++ " steps\n\n"
    ++ show s

-- Data Type, read and write

-- | 81 integer bitfields of possible values
--   in left-to-right, top-top bottom order
newtype Sudoku = Sudoku [Integer]


instance Eq Sudoku where
  Sudoku a == Sudoku b = a == b 

-- | Read as a whitespace-separated list of ints or _
--   (may be pretty-formatted if you whish)
instance Read Sudoku where
  readPrec = lift $ count boardsize readBox >>= return . Sudoku
  readListPrec = readListPrecDefault

-- | Show as a formatted square of numbers (known)
--   _ (all is possible), ? (multiple possibilities)
--   or x (no possibilities - error)
instance Show Sudoku where
  show (Sudoku s) = intercalate "\n\n" $
    map (intercalate "\n") $ chunksOf dimension $
    map (intercalate "  ") $ chunksOf dimension $
    map (intersperse ' ') $ chunksOf dimension $
    map showBox s

readBox :: ReadP Integer
readBox = munch1 (`elem` ('_':symbols)) <* skipSpaces
  >>= return . possible . map val
  where val '_' = allbits
        val c = case elemIndex c symbols of
          Just n -> nbit n
          Nothing -> error "should not happen"

showBox :: Integer -> Char
showBox b
  | n == 1 = symbols !! head (vals b)
  | n == groupsize = '_'
  | n == 0 = 'x'
  | otherwise = '.'
  where n = nbits b


-- Row, column, block extraction

rows :: Sudoku -> [[Integer]]
rows (Sudoku s) = chunksOf groupsize s

fromRows :: [[Integer]] -> Sudoku
fromRows = Sudoku. foldr1 (++)

cols :: Sudoku -> [[Integer]]
cols = transpose . rows

fromCols :: [[Integer]] -> Sudoku
fromCols = fromRows . transpose

blocks :: Sudoku -> [[Integer]]
blocks = blockDance . rows

fromBlocks :: [[Integer]] -> Sudoku
fromBlocks = fromRows . blockDance

blockDance :: [[Integer]] -> [[Integer]]
blockDance = map (foldr1 (++)) . foldr1 (++)
  . transpose . map (chunksOf dimension)
  . transpose . map (chunksOf dimension)


-- Resolution

solve :: Sudoku -> (Int, Sudoku)
solve s = solven (0, s)

solven :: (Int, Sudoku) -> (Int, Sudoku)
solven (n, s)
  | n >= 20 || s == s' = (n+1, s)
  | otherwise = solven (n+1, s')
  where s' = solve1 s

solve1 :: Sudoku -> Sudoku
solve1 = fromBlocks . map solveGroup . blocks
  . fromCols . map solveGroup . cols
  . fromRows . map solveGroup . rows

solveGroup :: [Integer] -> [Integer]
solveGroup = (`using` parList rseq) . map solveField . fields


solveField :: (Integer, [Integer]) -> Integer
solveField (f,c) = f .&. (allbits `xor` p)
  where l = map defined (combinations c)
        p = possible $ (allbits `xor` f) : l

check :: Sudoku -> String
check (Sudoku s)
  | all (==1) b = "solved"
  | any (==0) b = "failed"
  | otherwise = "undecided"
  where b = map nbits s

fields :: [Integer] -> [(Integer, [Integer])]
fields r = map (field r) [0 .. length r - 1]

field :: [Integer] -> Int -> (Integer, [Integer])
field r i = (r !! i, take i r ++ drop (i+1) r)

combinations :: [Integer] -> [[Integer]]
combinations [] = []
combinations (x:[]) = [[x]]
combinations (x:xs) = ([x] : combinations xs)
  ++ map (x:) (combinations xs)


possible :: [Integer] -> Integer
--possible = foldr1 (.|.)
possible (b:bs)
  | bs == [] || b == allbits = b
  | otherwise = b .|. possible bs

defined :: [Integer] -> Integer
defined r
  | nbits p > length r = 0
  | otherwise = p
  where p = possible r


-- Bitfield handling

dimension :: Int
symbols :: String

-- 3x3x3
--dimension = 3
--symbols = ['1' .. '9']

-- 4x4x4
dimension = 4
symbols = ['1' .. '9'] ++ ['a' .. 'g']

groupsize :: Int
groupsize = dimension * dimension

boardsize :: Int
boardsize = groupsize * groupsize

maxbit :: Int
maxbit = groupsize - 1

allbits :: Integer
allbits = (1 `shiftL` groupsize) - 1

bitn :: Integer -> Int -> Int
bitn b i = fromIntegral $ 1 .&. (b `shiftR` i)

nbit :: Int -> Integer
nbit i = 1 `shiftL` i

nbits :: Integer -> Int
nbits b = sum $ map (bitn b) [0..maxbit]

vals :: Integer -> [Int]
vals b = filter ((>0) . (bitn b)) [0..maxbit]
