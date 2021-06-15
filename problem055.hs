
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome [] = True
isPalindrome [x] = True
isPalindrome (x:xs) = (x == last xs) && (isPalindrome $ init xs)

isLychrel :: Integer -> Bool
isLychrel x = go x 0 where
    go x i
        | i >= 50                = True
        | isPalindrome (show x') = False
        | otherwise              = go x' (succ i)
        where x' = x + (read . reverse . show $ x)

problem55 = length $ filter isLychrel [1..9999]
main = print problem55
