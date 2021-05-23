
import Data.List (genericLength, maximumBy)
import Data.Function (on)
import Data.List.Ordered (minus, unionAll)

primes :: (Integral a) => [a]
primes = 2 : 3 : minus [5,7..] (unionAll [[p*p, p*p+2*p..] | p <- tail primes])

isPrime :: (Integral a) => a -> Bool
isPrime x = x == firstPrime where firstPrime = head . dropWhile (<x) $ primes


numQuadraticPrimes :: (Integral a) => a -> a -> a
numQuadraticPrimes a b = genericLength . takeWhile isPrime . map f $ [0..] where
    f n = n^2 + a*n + b


problem27 = fst . maximumBy (compare `on` snd) $ counts where
    counts = [(a * b, numQuadraticPrimes a b) | a <- [-999..999], b <- [-1000..1000]]

main = print problem27
