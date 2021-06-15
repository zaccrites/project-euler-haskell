
import Data.Ratio (numerator, denominator, (%))

continuedFraction :: Int -> Rational
continuedFraction n = (toRational 1) + (1 / denom n) where
    denom 1 = toRational 2
    denom n = (denom 1) + (1 / denom (pred n))

problem57 = length $ filter p $ map continuedFraction [1..1000] where
    p x = (length . show . numerator $ x) > (length . show . denominator $ x)

main = print problem57
