
problem53 = length $ filter (>1000000) [choose n r | n <- [1..100], r <- [1..n]] where
    choose n r = (fact n) / ((fact r) * (fact (n - r)))
    fact n = product [1..n]

main = print problem53
