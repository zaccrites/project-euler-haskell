
-- https://projecteuler.net/problem=34

module Main where

import Data.Char


fact :: Int -> Int
fact n
    | n <= 1    = 1
    | otherwise = n * fact (n - 1)


sumOfDigitFactorials :: Int -> Int
sumOfDigitFactorials n =
    let digits = map digitToInt . show $ n
        digitFactorials = map fact digits
    in sum digitFactorials


problem34 =
    let entries = [3..50000]
        sums = zip entries (map sumOfDigitFactorials entries)
        matches = [a | (a, b) <- sums, a == b]
    in sum matches


main = do putStrLn $ show problem34
