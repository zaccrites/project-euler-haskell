
import Data.List (sort)
import Data.List.Ordered (minus, union, unionAll)

primes :: (Integral a) => [a]
primes = 2 : 3 : minus [5, 7..] (unionAll [[p*p, p*p+2*p..] | p <- tail primes])

isPandigital :: (Show a, Integral a) => a -> Bool
isPandigital x = and $ zipWith (==) (sort $ show x) ['1'..'9']

-- If the sum of a number's digits is divisible by three,
-- the number itself is divisible by three.
--
--   sum [1..9] == 45, so there are no nine digit pandigital primes
--   sum [1..8] == 36, so there are no eight digit pandigital primes
--
-- The same is true for two, three, five, and six digits as well.
-- n-digit pandigital primes can only have one, four, or seven digits.
--
problem41 = last . filter isPandigital . takeWhile (<7654321) $ primes

main = print problem41
