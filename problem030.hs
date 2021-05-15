
module Main where

import Data.Char


sumOfDigitPowers :: Int -> Int
sumOfDigitPowers n =
    let digits = map digitToInt . show $ n
        digitPowers = map (^5) digits
    in sum digitPowers


problem30 =
    let entries = [2..300000]
        sums = zip entries (map sumOfDigitPowers entries)
        matches = [a | (a, b) <- sums, a == b]
    in sum matches


main = do putStrLn $ show problem30
