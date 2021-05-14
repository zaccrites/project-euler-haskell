
-- https://projecteuler.net/problem=2

module Main where


fibs :: [Integer]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)
-- fibs = 0 : scanl (+) 1 fibs


problem2 :: Integer
problem2 = sum . filter even . takeWhile (<4000000) $ fibs


main = do putStrLn $ show problem2
