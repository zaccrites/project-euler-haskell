
module Main where

import Data.Char

problem20 = sum . map digitToInt . show . product $ [1..100]

main = do putStrLn $ show problem20
