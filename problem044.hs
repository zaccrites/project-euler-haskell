
import Data.List (sort)

pentagonalNumbers :: (Integral a) => [a]
pentagonalNumbers = map f [1..] where f n = (n * (3*n - 1)) `div` 2

isPentagonalNumber :: (Integral a) => a -> Bool
isPentagonalNumber x = isNaturalNumber $ (sqrt (24.0 * (fromIntegral x) + 1) + 1) / 6
    where isNaturalNumber x = x - (fromIntegral $ floor x) < 0.00000001


problem44 = head . sort $ [
    d |
    pj <- take n pentagonalNumbers,
    pk <- take n pentagonalNumbers,
    let d = abs (pk - pj),
    isPentagonalNumber (pk + pj),
    isPentagonalNumber (d)] where n = 5000

main = print problem44
