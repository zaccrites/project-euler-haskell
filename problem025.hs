
-- https://projecteuler.net/problem=25

module Main where

fibs :: [Integer]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

problem25 =
    let termLengths = zip [0..] (map (length . show) $ fibs)
        longTerms = dropWhile (\(_, len) -> len < 1000) termLengths
    in fst . head $ longTerms


main = do putStrLn $ show problem25
