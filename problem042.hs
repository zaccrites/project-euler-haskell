
import Data.Char (ord, toUpper)
import System.IO (readFile)

split :: (a -> Bool) -> [a] -> [[a]]
split p xs = case dropWhile p xs of
    []  -> []
    xs' -> part : split p xs'' where
        (part, xs'') = break p xs'

getWords :: String -> [String]
getWords = map (filter (/='"')) . split (==',')

wordValue :: (Integral a) => String -> a
wordValue [] = 0
wordValue (x:xs) = letterValue + wordValue xs where
    letterValue = fromIntegral $ (ord . toUpper $ x) - (ord 'A') + 1


triangleNumbers :: (Integral a) => [a]
triangleNumbers = map f [1..] where f n = n * (n + 1) `div` 2

isTriangleNumber :: (Integral a) => a -> Bool
isTriangleNumber x = (==x) . head . dropWhile (<x) $ triangleNumbers


problem42 contents = length . filter isTriangleNumber . map wordValue $ getWords contents

main = do
    contents <- readFile "resources/p042_words.txt"
    print $ problem42 contents
