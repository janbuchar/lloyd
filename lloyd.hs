-- Takes the dimensions of the board and a list describing the board.
-- Returns a list of fields to swap with the zero field
solve :: Int -> Int -> [Int] -> [Int]
solve x y board 
        | length board /= x * y = noSolution
        | not (elem 0 board) = noSolution
        | otherwise = [0]

-- A special value that is returned iff the puzzle has no solution
noSolution :: [Int]
noSolution = [-1]

