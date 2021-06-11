
import Data.List.Ordered (minus, union, unionAll)


primes :: (Integral a) => [a]
primes = 2 : 3 : minus [5,7..] (unionAll [[p*p, p*p+2*p..] | p <- tail primes])

isPrime :: (Integral a) => a -> Bool
isPrime x = x == firstPrime where firstPrime = head . dropWhile (<x) $ primes


digitSwaps :: Int -> [[Int]]
digitSwaps x = [[read ys | n <- ['0'..'9'], let ys = replace n bs, head ys /= '0'] | bs <- bools] where
    replace n = zipWith (\d b -> if b then n else d) digits
    bools = filter (not . and) $ filter or $ sequence $ replicate (length digits) [False, True]
    digits = show x


problem51 = head . head $ filter ((==8) . length) $ concatMap (map (filter isPrime)) $ map digitSwaps primes' where
    primes' = dropWhile (<56000) primes

main = print problem51
