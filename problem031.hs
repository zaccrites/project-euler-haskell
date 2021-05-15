
-- https://projecteuler.net/problem=31

module Main where

-- One extra solution for a single £2 coin
problem31 = 1 + length [
    ":)" |
    onep    <- [0 .. 200],
    twop    <- [0 .. (200 - onep) `div` 2],
    fivep   <- [0 .. (200 - onep - 2 *  twop) `div` 5],
    tenp    <- [0 .. (200 - onep - 2 * twop - 5 * fivep) `div` 10],
    twentyp <- [0 .. (200 - onep - 2 * twop - 5 * fivep - 10 * tenp) `div` 20],
    fiftyp  <- [0 .. (200 - onep - 2 * twop - 5 * fivep - 10 * tenp - 20 * twentyp) `div` 50],
    oneP    <- [0 .. (200 - onep - 2 * twop - 5 * fivep - 10 * tenp - 20 * twentyp - 50 * fiftyp) `div` 100],
    200 == 1 * onep + 2 * twop + 5 * fivep + 10 * tenp + 20 * twentyp + 50 * fiftyp + 100 * oneP]


main = do putStrLn $ show problem31
