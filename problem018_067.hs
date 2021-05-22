
-- Problems 18 and 67 are the same, just with different inputs.

maxSumPath :: (Integral a) => [[a]] -> a
maxSumPath rows = head . go $ rows where
    go (upper:lower:[]) = zip' upper lower
    go (row:rows') = go [row, go rows']

    -- Special zip that adds the value from the first list to the greater
    -- of the next two values from the second list. The second value is
    -- put back onto the list for the next iteration.
    zip' _ [] = []
    zip' [] _ = []
    zip' (x:xs) (y1:y2:ys) = x + (max y1 y2) : zip' xs (y2:ys)


readTriangle :: (Integral a, Read a) => String -> [[a]]
readTriangle = map getNumbers . lines where
    getNumbers line = map read $ words line


problem :: String -> FilePath -> IO ()
problem name path = do
    contents <- readFile path
    let answer = maxSumPath . readTriangle $ contents
    putStrLn $ name ++ ": " ++ show answer


problem18 = problem "Problem 18" "resources/p018_triangle.txt"
problem67 = problem "Problem 67" "resources/p067_triangle.txt"
main = sequence [problem18, problem67]
