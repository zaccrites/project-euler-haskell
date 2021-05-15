
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


-- One extra solution for a single £2 coin
problem31' = 1 + countSolutions 0 100 where
    countSolutions acc coinValue
        | acc == 200 = 1  -- found a solution!
        | acc >  200 = 0  -- not a solution, combined value is too high

        -- Limit the coins used in finding additional solutions to those smaller than the current coin.
        -- That is, if our current coin is 20p, don't bother trying to use 50p coins as those
        -- will be tried later. Recursively add smaller and smaller coins until the value reaches
        -- or exceeds 200p, then count the valid solutions. Once the number of current coin values
        -- added exceeds 200p, we move onto the next coin value looking for a solution there.
        | let getInnerSolutions nextCoinValue = countSolutions (acc + nextCoinValue) nextCoinValue,
          otherwise = sum . map getInnerSolutions . filter (<=coinValue) $ [1, 2, 5, 10, 20, 50, 100]


main = do
    putStrLn $ show problem31
    putStrLn $ show problem31'
