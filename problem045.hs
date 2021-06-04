
isPentagonalNumber :: (Integral a) => a -> Bool
isPentagonalNumber x = isInteger $ (sqrt (24.0 * (fromIntegral x) + 1) + 1) / 6

isHexagonalNumber :: (Integral a) => a -> Bool
isHexagonalNumber x = isInteger $ (sqrt (8.0 * (fromIntegral x) + 1) + 1) / 4

isInteger :: Double -> Bool
isInteger x = x - (fromIntegral $ floor x) < 0.00000001

triangleNumbers :: (Integral a) => [a]
triangleNumbers = map f [1..] where f n = n * (n + 1) `div` 2

problem45 = head . filter p . dropWhile (<=40755) $ triangleNumbers
    where p n = isPentagonalNumber n && isHexagonalNumber n

main = print problem45
