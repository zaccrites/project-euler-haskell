
-- This is a brute-force solution. It could be made much faster by exploiting properties
-- of abundant numbers. For example, all multiples of an abundant number are also abundant.

import Data.List (subsequences, nub)
import Data.List.Ordered (minus, union, unionAll)

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

sumOfProperDivisors :: (Integral a) => a -> a
sumOfProperDivisors x = sum . filter (/=x) . factors $ x

ourAbundantNumbers :: (Integral a) => [a]
ourAbundantNumbers = takeWhile (<=maxValue) . filter isAbundantNumber $ map fromIntegral [1..] where
    isAbundantNumber x = sumOfProperDivisors x > x

abundantSums :: (Integral a) => [a]
abundantSums = [a + b | a <- ourAbundantNumbers, b <- ourAbundantNumbers]

nonAbundantSums :: (Integral a) => [a]
nonAbundantSums = takeWhile (<=maxValue) . filter (`notElem` abundantSums) $ map fromIntegral [1..]

maxValue :: (Integral a) => a
maxValue = 20161

problem28 = sum nonAbundantSums

main = print problem28
