
import Data.List (nub, group)
import Data.List.Ordered (minus, union, unionAll)

primes :: (Integral a) => [a]
primes = 2 : 3 : minus [5,7..] (unionAll [[p*p, p*p+2*p..] | p <- tail primes])

primeFactors :: (Integral a) => a -> [a]
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

-- For this problem "2" and "2^2" are distinct prime factors,
-- so we convert e.g. [2, 2, 2, 3, 3, 5] into [(2, 3), (3, 2), (5, 1)]
groupPrimeFactors :: (Integral a) => a -> [(a, Int)]
groupPrimeFactors = map (\ns -> (head ns, length ns)) . group . primeFactors


windows :: [a] -> [[a]]
windows (x1:x2:x3:x4:xs) = [x1, x2, x3, x4] : windows (x2:x3:x4:xs)


problem47 = fst . head . head . filter p . windows . map f $ [2..] where
    f x = (x, groupPrimeFactors $ x)
    p xs = allLength4 && allUniqueFactors where
        allLength4 = (all ((==4) . length) $ factors)
        allUniqueFactors = ((==16) . length . nub . concat $ factors)
        factors = map snd xs

main = print problem47
