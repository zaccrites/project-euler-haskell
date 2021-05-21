
import Data.List (nub)
import Data.List.Ordered (minus, union, unionAll)


primes :: (Integral a) => [a]
primes = 2 : 3 : minus [5,7..] (unionAll [[p*p, p*p+2*p..] | p <- tail primes])

primes' :: (Integral a) => [a]
primes' = takeWhile (<1000000) primes


rotate :: [a] -> [a]
rotate [] = []
rotate [x] = [x]
rotate (x:xs) = xs ++ [x]

rotations :: [a] -> [[a]]
rotations xs = xs : go xs 1 where
    go xs' count
        | count == length xs' = []
        | otherwise           = xs'' : (go xs'' (count + 1))
        where xs'' = rotate xs'


digitRotations :: (Show a, Read a, Integral a) => a -> [a]
digitRotations = nub . map read . rotations . show

isCircularPrime :: (Show a, Read a, Integral a) => a -> Bool
isCircularPrime = all (`elem` primes') . digitRotations


problem35 = length . filter isCircularPrime $ primes'

main = print problem35
