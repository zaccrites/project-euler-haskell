
import Data.Bits ((.&.), shiftR)


isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome [] = True
isPalindrome [x] = True
isPalindrome (x:xs) = (x == (last xs)) && (isPalindrome . init $ xs)


showBin :: Int -> String
showBin x
    | x == 0       = "0"
    | x == 1       = "1"
    | x .&. 1 == 1 = rest ++ "1"
    | otherwise    = rest ++ "0"
    where rest = showBin $ shiftR x 1


problem36 = sum . filter p . takeWhile (<1000000) $ [1..] where
    p = and . map isPalindrome . sequenceA [show, showBin]

main = print problem36
