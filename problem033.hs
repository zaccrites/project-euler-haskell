
fractionsEq :: (Integral a) => (a, a) -> (a, a) -> Bool
fractionsEq (n1, d1) (n2, d2) = n1 * d2 == n2 * d1

{-
    On reading the problem text I assumed that only the ones digit of the
    numerator and the tens digit of the denominator could cancel.
    I guess that was just luck, as other responses in the problem thread
    were looking for all solutions eliminating a common digit from either
    the ones or tens digit spot in both the numerator and denominator.
-}
fractions = [
    (n, d) | d <- [11,12..99], n <- [10,11..d-1],
    let (n1, n2) = n `divMod` 10, let (d1, d2) = d `divMod` 10,
    n2 == d1, fractionsEq (n, d) (n1, d2)]


-- The denominator came out to exactly a multiple of the numerator,
-- so reducing the fraction is just division here.
problem33 = d `div` n where
    (n, d) = foldl (\(an, ad) (n, d) -> (an * n, ad * d)) (1, 1) fractions

main = print problem33
