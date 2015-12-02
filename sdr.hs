import Data.Bits
import Control.Monad
import Data.Word
import Data.Char
import qualified Data.ByteString as B 
import Control.Monad.Reader
import Text.ParserCombinators.Parsec

csvFile = endBy line eol
line = sepBy cell (char ',')
cell = quotedCell <|> many (noneOf ",\n\r")

quotedCell = 
    do char '"'
       content <- many quotedChar
       char '"' <?> "quote at end of cell"
       return content

quotedChar =
        noneOf "\""
    <|> try (string "\"\"" >> return '"')

eol =   try (string "\n\r")
    <|> try (string "\r\n")
    <|> string "\n"
    <|> string "\r"
    <?> "end of line"

parseCSV :: String -> Either ParseError [[String]]
parseCSV input = parse csvFile "(unknown)" input

main =
    do c <- getContents
       case parse csvFile "(stdin)" c of
            Left e -> do putStrLn "Error parsing input:"
                         print e
            Right r -> mapM_ print r





data MyContext = MyContext {
	mass :: Float,
	velocity :: Float
} deriving Show

file = "C:\\Users\\Daniel\\Documents\\development\\code\\sdr.txt"

	
computation :: Reader [Neuron] (Float)
computation = do
	x <- asks (map threshhold)
	return (sum x)

data Neuron = Neuron { weights :: [Float],
					   threshhold :: Float,
					   payload :: [[Word8]]
					} deriving (Show) 

tneuron1 = Neuron [5,-1,2,1,-4] 5 [[5],[-1],[2],[1],[-4]]
tneuron2 = Neuron [-1,2,5,6, 0] 6 [[-1],[2],[5],[6],[0]]
nspace = [tneuron1, tneuron2]

address :: (Num a, Ord a, Integral b) => [a] -> [b]
address x = map toBit x
	where toBit y
		| y <= 0    = 0
		| otherwise = 1

nAddress :: (Integral b) => Neuron -> [b]
nAddress = (address . weights)

vProduct :: Num a => [a] -> [a] -> a
vProduct x y = sum $ zipWith (*) x y

writeAtNeuron :: [Word8] -> Neuron -> Neuron 
writeAtNeuron x y = Neuron (weights y) (threshhold y) (x:(payload y))

writeNeurons :: [Word8] -> [Neuron] -> Int -> [Int] -> [Neuron]
writeNeurons load space distance addr =
	let ham x y = fromIntegral $ hamming $ zipWith (==) x y
	in map (writeAtNeuron load) [target | target <- space, ham addr (nAddress target) < distance]


hamming :: [Bool] -> Int
hamming x = sum $ map toNum x
	where toNum y
		| y == True = 0
		| otherwise = 1



readAtAddress :: [Neuron] -> [Int] -> [[Word8]]
readAtAddress space addr =
	concat [payload(target) | target <- space, weights(target) `vProduct` (map fromIntegral addr) > 0]



expPair :: (a -> b) -> a -> (a,b)
expPair = \x -> (\y -> (y, x y) )

loadInput = do
	s <- readFile file
	t <- return (parseCSV s)
	print t

csvToNeurons :: [[[Word8]]] -> [Neuron] -> [Int] -> [Neuron]
csvToNeurons infile space address = writeNeurons (concat . concat $ infile) space 5 address  



