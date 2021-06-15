
import Data.Char (digitToInt)

problem56 = maximum [sum . map digitToInt . show $ a^b | a <- [2..99], b <- [1..99]]

main = print problem56
