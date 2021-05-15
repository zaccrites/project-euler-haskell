
-- https://projecteuler.net/problem=4

module Main where

import Data.List
import Data.Function


collatz :: (Integral a) => a -> [a]
collatz 1 = [1]
collatz n = n : collatz (next n) where
    next n
        | even n = n `div` 2
        | odd  n = (3 * n) + 1


problem14 =
    let inputs = takeWhile (<1000000) [1..]
        chainLengths = zip inputs (map (length . collatz) $ inputs)
        longestChain = maximumBy (compare `on` snd) chainLengths
    in fst longestChain


main = do putStrLn $ show problem14
