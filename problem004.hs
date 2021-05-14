
-- https://projecteuler.net/problem=4

module Main where


isPalindrome :: String -> Bool
isPalindrome [] = True
isPalindrome [x] = True
isPalindrome (x:xs) = (x == (last xs)) && (isPalindrome . init $ xs)


problem4 :: Integer
problem4 =
    let numbers = [999, 998 .. 100]
    in maximum . filter (isPalindrome . show) $ [x * y | x <- numbers, y <- numbers]


main = do putStrLn $ show problem4
