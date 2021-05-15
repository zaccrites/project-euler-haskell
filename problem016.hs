
-- https://projecteuler.net/problem=16

module Main where

import Data.Char

problem16 = sum . map digitToInt . show $ 2^1000

main = do putStrLn $ show problem16
