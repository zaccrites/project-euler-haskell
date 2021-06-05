
import Data.Function (on)
import Data.List (sort, sortBy, groupBy, subsequences)
import Data.List.Ordered (minus, union, unionAll)

primes :: (Integral a) => [a]
primes = 2 : 3 : minus [5,7..] (unionAll [[p*p, p*p+2*p..] | p <- tail primes])

isPrime :: (Integral a) => a -> Bool
isPrime x = x == firstPrime where firstPrime = head . dropWhile (<x) $ primes

problem49 = (a * 10^8) + (b * 10^4) + c where
    [a, b, c] = head . filter (not . elem 1487) $ arithmeticGroups
    arithmeticGroups = filter (\[a, b, c] -> (c - b) == (b - a)) groupsOfThree
    groupsOfThree = concatMap (filter((3==) . length) . subsequences) $ groupedPrimes
    groupedPrimes = groupBy (\a b -> f a == f b) . sortBy (compare `on` f) $ fourDigitPrimes where f = sort . show
    fourDigitPrimes = takeWhile (<=9999) . dropWhile (<=999) $ primes

main = print problem49
