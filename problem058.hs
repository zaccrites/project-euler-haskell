
import Data.Ratio (Ratio, (%))
import Data.List.Ordered (minus, union, unionAll)

primes :: (Integral a) => [a]
primes = 2 : 3 : minus [5,7..] (unionAll [[p*p, p*p+2*p..] | p <- tail primes])

isPrime :: (Integral a) => a -> Bool
isPrime x = x == firstPrime where firstPrime = head . dropWhile (<x) $ primes


gridCorners :: Int -> [Int]
gridCorners n = map ((a-) . (2*n*)) [3,2..0] where
    a = (2*n + 1) ^ 2


-- This solution takes about 42 minutes
diagonalPrimeRatios :: [Ratio Int]
diagonalPrimeRatios = go 1 0 where
    go n acc = acc' % (4 * n + 1) : go (succ n) acc' where
        x = length $ filter isPrime $ gridCorners n
        acc' = acc + x


problem58 = gridSideLength $ fst . head $ dropWhile p $ zip [1..] diagonalPrimeRatios where
    p = (>= 1%10) . snd
    gridSideLength n = 2 * n + 1

main = print problem58
m = main
