
-- https://projecteuler.net/problem=4

module Main where


problem9 = take 1 [
    a * b * c |
    c <- [1..], b <- [1..c], a <- [1..b],
    a^2 + b^2 == c^2, a + b + c == 1000]

main = do putStrLn $ show problem9
