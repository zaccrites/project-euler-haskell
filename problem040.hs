
import Data.Char (digitToInt)

problem40 = product . map snd . filter (\(i,_) -> i `elem` [10^x|x<-[0..6]]) . take 1000000 . zip [1..] . map digitToInt . concat . map show $ [1..]

problem40' = product $ map (digits !!) indices where
    indices = [(10 ^ i) - 1 | i <- [0..6]]
    digits = map digitToInt . concatMap show $ [1..]

main = print problem40'
