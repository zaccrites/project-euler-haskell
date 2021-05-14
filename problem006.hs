
-- https://projecteuler.net/problem=2

module Main where

problem6 = squareOfSum - sumOfSquares where
    sumOfSquares = sum . map (^2) $ [1..100]
    squareOfSum = (sum [1..100]) ^ 2

main = do putStrLn $ show problem6
