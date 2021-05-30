
import Data.List (nub, permutations)


partitionSegmentLengths :: Int -> [(Int, Int)]
partitionSegmentLengths n = [
    (a, b) | c <- [1..n], a <- [1..c], b <- [1..c],
    -- ALl digits used
    a + b + c == n,
    -- A product has as many digits as multiplicand and multiplier put together,
    -- plus or minus one digit.
    c == a + b || c == a + b + 1 || c == a + b - 1]


partition :: (Int, Int) -> [a] -> ([a], [a], [a])
partition (a, b) xs = (as, bs, cs) where
    as = take a xs
    bs = take b . drop a $ xs
    cs = take c . drop (a + b) $ xs
    c = subtract (a + b) . length $ xs


products :: (Integral a) => [a] -> [a]
products = map f . filter p . partitions where
    f (_, _, c) = c
    p (a, b, c) = a * b == c


partitions :: (Integral a) => [a] -> [(a, a, a)]
partitions digits = [
    (digitsToNumber as, digitsToNumber bs, digitsToNumber cs) |
    digits' <- permutations digits,
    (a, b) <- partitionSegmentLengths . length $ digits',
    let (as, bs, cs) = partition (a, b) digits']


digitsToNumber :: (Integral a) => [a] -> a
digitsToNumber digits = sum $ zipWith (\x y -> x * 10^y) (reverse digits) [0..]


problem32 = sum . nub . products $ [1..9]

main = print problem32
