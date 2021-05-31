
import Data.List (sort)


makeNum :: (Show a, Integral a) => a -> String
makeNum x = go "" 1 where
    go acc i
        | length next > 9 = acc
        | otherwise       = go next (succ i)
        where next = acc ++ show (x * i)


pandigitals :: (Read a, Integral a) => [a]
pandigitals = map read . filter p . map makeNum $ source where
    p xs = length xs == 9 && sort xs == "123456789"
    source = [9] ++ [99,98..90] ++ [999,998..900] ++ [9999,9998..9000] ++ [99999,99998..90000]


problem38 = maximum pandigitals

main = print problem38
