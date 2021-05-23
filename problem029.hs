
import Data.List (nub)

problem29 = length . nub $ [a ^ b | a <- [2..100], b <- [2..100]]

main = print problem29
