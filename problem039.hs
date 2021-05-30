
import Data.Function (on)
import Data.List (maximumBy)


solutions :: (Integral a) => a -> [(a, a, a)]
solutions p = [
    (a, b, c) | c <- [1..p `div` 2], b <- [1..c], a <- [1..b],
    a + b + c == p, a^2 + b^2 == c^2]


problem39 = fst . maximumBy (compare `on` snd) $ solutionCounts where
    solutionCounts = map (\p -> (p, length . solutions $ p)) [1..1000]

main = print problem39
