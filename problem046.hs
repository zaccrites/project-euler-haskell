
import Data.List.Ordered (minus, union, unionAll)

primes :: (Integral a) => [a]
primes = 2 : 3 : minus [5,7..] (unionAll [[p*p, p*p+2*p..] | p <- tail primes])

oddComposites :: (Integral a) => [a]
oddComposites = minus [3,5..] primes

doubleSquares :: (Integral a) => [a]
doubleSquares = map ((*2) . (^2)) [1..]

problem46 = head . filter p $ oddComposites where
    p n = null [1 | p <- takeWhile (<n) primes, ds <- takeWhile (<n) doubleSquares, n == p + ds]

main = print problem46
