
fact :: (Integral a) => a -> a
fact n = product [1..n]

numPaths :: (Integral a) => a -> a
numPaths size = num `div` den where
    num = fact (2 * (size - 1))
    den = fact (size - 1) ^ 2

problem15 = numPaths (20 + 1)

main = print problem15
