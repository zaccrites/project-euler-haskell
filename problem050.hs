
import Data.Function (on)
import Data.List (maximumBy)
import Data.List.Ordered (minus, union, unionAll)


primes :: (Integral a) => [a]
primes = 2 : 3 : minus [5,7..] (unionAll [[p*p, p*p+2*p..] | p <- tail primes])

isPrime :: (Integral a) => a -> Bool
isPrime x = x == firstPrime where firstPrime = head . dropWhile (<x) $ primes

-- Initial slow solution: find all prime sums of consecutive prime sequences
-- after some initial prime, counting the number of terms in each sum.

-- Find prime sums of sequences of prime numbers after some starting point.
primeSequenceSums :: (Integral a) => Int -> [(a, Int)]
primeSequenceSums n = go 0 1 (drop n primes) where
    go acc count (x:xs)
        | acc' >= 1000000  = []
        | isPrime acc'     = (acc', count) : rest
        | otherwise        = rest
        where acc' = acc + x
              rest = go acc' (succ count) xs

problem50 = maximumBy (compare `on` snd) . concat . takeWhile (not . null) $ map primeSequenceSums [0..]



-- Alternative fast solution: calculate the sum of all consecutive primes
-- (starting from zero) up to the limit. Then for each prime entry `psum1`
-- in the list (corresponding to prime number `n1`), subtract earlier
-- partial sums `psum2` (corresponding to prime number `n2`) starting from the
-- beginning of the list. If `psum1 - psum2` is prime, then the sequence
-- of primes corresponding to the entries between `psum2` and `psum1`
-- (i.e. `n2` through `n1`, the count of which is `n1 - n2`)
-- sum to `psum2 - psum1` and are thus solution candidates.

partialSums :: (Integral a) => [(Int, a)]
partialSums = go primes 0 1 where
    go (x:xs) acc n
        | acc' >= 1000000 = []
        | otherwise = (n, acc') : go xs acc' (succ n)
        where acc' = acc + x


partialSumLength :: (Integral a) => (Int, a) -> (a, Int)
partialSumLength (n1, psum1)
    | isPrime psum1 = (psum1, n1)
    | otherwise = go partialSums where
        go ((n2, psum2):ps)
            | isPrime psum' = (psum', n1 - n2)
            | otherwise = go ps
            where psum' = psum1 - psum2


problem50' = fst . maximumBy (compare `on` snd) $ map partialSumLength partialSums

main = do
    print problem50'
    print problem50
