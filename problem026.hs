
import Data.List (maximumBy)
import Data.Function (on)

magnitude :: (Integral a) => a -> a
magnitude = floor . logBase 10 . fromIntegral

-- We only have to deal with numbers of small magnitude in this problem.
isPowerOf10 :: (Integral a) => a -> Bool
isPowerOf10 x = x `elem` [1, 10, 100, 1000, 10000, 100000, 1000000]

cycleLength :: (Integral a) => a -> a
cycleLength d = go 1 [] where
    go m r'
        -- Magnitude of numerator is too small, try again with m+1
        | q == 0  = go (m + 1) []
        -- No cycle, evenly divisible at some 10^m (e.g. 1000/8 = 125)
        | r == 0  = 0
        -- Simple cycle, repeats at some 10^m (e.g. 10/3 = 3 w/ remainder 1)
        | isPowerOf10 r  = m - (magnitude r)
        -- If the remainder repeats after increasing the magnitude
        -- (see otherwise pattern) then we've reached the repeating part after
        -- a fixed prefix (e.g. 10/6 = 1 w/ remainder 4, 100/6 = 16 w/ remainder 4).
        -- The length of the prefix is derived by comparing the magnitude of
        -- the numerator and denominator of the second division operation.
        | r `elem` r'  = m - (magnitude d) - 1
        -- Otherwise the results are inconclusive,
        -- so try again with a numerator of greater magnitude.
        | otherwise  = go (m + 1) (r:r')
        where
            (q, r) = (10 ^ m) `divMod` d

problem26 = fst . maximumBy (compare `on` snd) . map (\d -> (d, cycleLength d)) $ [1..999]

main = print problem26
