
import Data.List.Ordered (minus, union, unionAll)

primes :: (Integral a) => [a]
primes = 2 : 3 : minus [5,7..] (unionAll [[p*p, p*p+2*p..] | p <- tail primes])

isPrime :: (Integral a) => a -> Bool
isPrime x = x == firstPrime where firstPrime = head . dropWhile (<x) $ primes


truncatablePrimes :: (Show a, Integral a) => [a]
truncatablePrimes = filter p primes where
    p x = let digits = show x in
        (length digits > 1) &&
        (and $ primeLeftTruncations digits) &&
        (and $ primeRightTruncations digits)
    primeLeftTruncations digits = map (isPrime . read) . leftTruncations $ digits
    primeRightTruncations digits = map (isPrime . read) . rightTruncations $ digits


leftTruncations :: [a] -> [[a]]
leftTruncations xs = xs : go xs where
    go [] = []
    go (_:[]) = []
    go (_:xs) = xs : go xs


rightTruncations :: [a] -> [[a]]
rightTruncations xs = xs : go xs where
    go xs = let xs' = init xs
            in case xs' of
                [] -> []
                _  -> [xs'] ++ go xs'


problem37 = sum . take 11 $ truncatablePrimes

main = print problem37
