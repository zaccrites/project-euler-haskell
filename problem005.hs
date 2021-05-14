
-- https://projecteuler.net/problem=5

import Data.List
import Data.Maybe


-- This will *eventually* (after ~12 minutes) find the answer.
-- Better to find the prime factorizations and use those instead.
problem5 :: Integer
problem5 =
    let evenlyDivisible x = all (\factor -> x `mod` factor == 0) [1..20]
    -- in fromJust . find evenlyDivisible $ [1..]
    in fromJust . find evenlyDivisible $ [232792560]


main = do putStrLn $ show problem5


{-

Prime factorizations:

 1 : N/A
 2 : 2
 3 : 3
 4 : 2 2
 5 : 5
 6 : 2 3
 7 : 7
 8 : 2 2 2
 9 : 3 3
10 : 2 5
11 : 11
12 : 2 2 3
13 : 13
14 : 2 7
15 : 3 5
16 : 2 2 2 2
17 : 17
18 : 2 3 3
19 : 19
20 : 2 2 5

Extracting the longest prime chains:
2 2 2 2
3 3
5
7
11
13
17
19

The overall product:
232,792,560

-}
