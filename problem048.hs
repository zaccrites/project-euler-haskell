
module Main where

problem48 = let result = show . sum $ [n^n | n <- [1..1000]] in drop (length result - 10) result

main = do putStrLn $ problem48
