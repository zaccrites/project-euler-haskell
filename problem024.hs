
import Data.List (sort, permutations)

problem24 = last . take 1000000 . sort . permutations $ ['0'..'9']

main = putStrLn problem24
