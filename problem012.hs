
import Data.List (subsequences, nub)
import Data.List.Ordered (minus, union, unionAll)


triangleNumbers :: (Integral a) => [a]
triangleNumbers = 1 : 3 : zipWith (+) [3..] (tail triangleNumbers)


primes :: (Integral a) => [a]
primes = 2 : 3 : minus [5,7..] (unionAll [[p*p, p*p+2*p..] | p <- tail primes])

primeFactors :: (Integral a) => a -> [a]
primeFactors x = factorize [x] where
    factorize [] = []
    factorize (x:xs)
        | x == 1    = xs
        | otherwise = factor : factorize (x `div` factor : xs) where factor = findPrimeFactor x
    findPrimeFactor x = head . dropWhile (not . isFactorOf x) $ primes
    isFactorOf n x = n `mod` x == 0

factors :: (Integral a) => a -> [a]
factors = (1:) . nub . map product . filter ((/=0) . length) . subsequences . primeFactors


problem12 = head . dropWhile ((<=500) . length . factors) $ triangleNumbers

main = print problem12
