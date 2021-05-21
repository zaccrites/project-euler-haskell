
import Data.List (sort)
import Data.Char (ord, toUpper)
import System.IO (readFile)


split :: (a -> Bool) -> [a] -> [[a]]
split p xs = case dropWhile p xs of
    []  -> []
    xs' -> part : split p xs'' where
        (part, xs'') = break p xs'


getNames :: String -> [String]
getNames = map (filter (/='"')) . split (==',')


nameAlphaValue :: (Integral a) => String -> a
nameAlphaValue = sum . map letterScore where
    letterScore c = fromIntegral ((ord . toUpper $ c) - (ord 'A') + 1)


problem22 contents =
    let names = sort . getNames $ contents
        nameAlphaValues = map nameAlphaValue names
        nameScores = zipWith (*) [1..] nameAlphaValues
    in sum nameScores

main = do
    contents <- readFile "resources/p022_names.txt"
    print $ problem22 contents
