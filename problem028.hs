
diagonals :: (Integral a) => [a]
diagonals = 1 : go 1 2 where
    go layer start
        | width > 1001 = []
        | otherwise    = corners ++ go (succ layer) nextStart where
            width = 2 * layer + 1
            skips = width - 2
            corners = take 4 . map (start+) $ [skips, skips*2+1..]
            nextStart = succ . last $ corners

problem28 = sum diagonals

{-
    Alternatively, directly sum the values at the corners.
    As mentioned in the solutions thread you can derive the values
    at the corners of layer n:

        corner[0] = (2n + 1)^2 =  4n^2 + 4n + 1
        corner[1] = corner[0] - 2n
        corner[2] = corner[0] - 4n
        corner[3] = corner[0] - 6n

    The sum of all four corners is therefore

        4(4n^2 + 4n + 1) - 12n =  16n^2 + 4n + 4

    The width of a layer is (2n + 1), so we need 500 layers to
    get a 1001x1001 grid.

    Add 1 for the center of the grid.
-}
problem28' = (1+) . sum . map f $ [1..500] where f n = 16*n^2 + 4*n + 4


main = print problem28
