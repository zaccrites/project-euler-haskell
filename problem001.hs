
-- https://projecteuler.net/problem=1

module Main where

problem1 :: Integer
problem1 =
    let isMultipleOf n x = x `mod` n == 0
        isOurMultiple x = (isMultipleOf 3 x) || (isMultipleOf 5 x)
        multiples = takeWhile (<1000) $ filter isOurMultiple [1..]
    in sum $ multiples


main = do putStrLn $ show problem1
