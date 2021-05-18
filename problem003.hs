
import Data.List.Ordered (minus, union, unionAll)

primes :: [Integer]
primes = 2 : 3 : minus [5,7..] (unionAll [[p*p, p*p+2*p..] | p <- tail primes])


primeFactors x = factorize [x] where
    factorize [] = []
    factorize (x:xs)
        -- If we just pushed 1 onto the list of factors then we're done.
        | x == 1    = xs
        -- Otherwise push the newly found prime factor onto the list and restart
        -- the search by dividing the head by the new factor.
        | otherwise = factor : factorize (x `div` factor : xs) where factor = findPrimeFactor x
    -- Find the first (smallest) prime factor
    findPrimeFactor x = head . dropWhile (not . isFactorOf x) $ primes
    isFactorOf n x = n `mod` x == 0


problem3 = last . primeFactors $ 600851475143

main = putStrLn $ show problem3
