
import Data.List (sort, nub)

problem52 = head [x | x <- [1..], let ys = nub [sort . show $ x*y | y <- [1..6]], length ys == 1]

main = print problem52'
