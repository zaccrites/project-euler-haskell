
import Data.List (tails, isPrefixOf)
import Data.Char (chr, ord)
import Data.Maybe (catMaybes)
import Data.Bits (xor)

type Ciphertext = [Int]
type Plaintext = String
type Key = [Int]


decrypt :: Ciphertext -> Key -> Maybe Plaintext
decrypt xs key = sequenceA $ map f $ zip xs (cycle key) where
    f (x, y)
        | isPlaintextChar = Just x'
        | otherwise       = Nothing
        where x' = chr $ x `xor` y
              isPlaintextChar = x' `elem` [' '..'~']


split :: (a -> Bool) -> [a] -> [[a]]
split p xs = case dropWhile p xs of
    []  -> []
    xs' -> part : split p xs'' where
        (part, xs'') = break p xs'


problem59 contents = sum $ map ord message where
    message = head . filter p $ catMaybes $ map (decrypt ciphertext) potentialKeys
    p = or . map (isPrefixOf "Euler") . tails
    ciphertext = map read $ split (==',') contents
    potentialKeys = sequenceA $ replicate 3 $ map ord ['a'..'z']

main = do
    contents <- readFile "resources/p059_cipher.txt"
    print $ problem59 contents
