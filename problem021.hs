
module Main where

import Data.List


d :: Int -> Int
d n = sum properDivisors where
    properDivisors = filter (\x -> n `mod` x == 0) [1..n-1]


problem21 =
    let entries = takeWhile (<10000) [0..]
        ds = zip entries (map d entries)
        amicablePairs = [(a, b) | (a, ad) <- ds, (b, bd) <- ds, a /= b, ad == b, bd == a]
        amicableNumbers = nub . foldr (\(a, b) acc -> a:b:acc) [] $ amicablePairs
    in sum amicableNumbers


main = do putStrLn $ show problem21
