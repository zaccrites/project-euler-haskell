
import Data.List (permutations)

p :: String -> Bool
p s = go [17, 13, 11, 7, 5, 3, 2] 7 where
    go [] _     = True
    go (d:ds) i = ((==0) . (`mod` d) . read . take 3 . drop i $ s) && go ds (pred i)


problem43 = sum . map read . filter p $ permutations ['0'..'9']

main = print problem43
