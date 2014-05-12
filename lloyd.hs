import Data.List (elemIndex)

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

-- The goal state for given dimensions
goalState :: Int -> Int -> [Int]
goalState x y = [1..x * y - 1] ++ [0]

-- Generate state transitions for given state
nextStates :: Int -> Int -> [Int] -> [[Int]]
nextStates x y s = []
        ++ (if (mod zPos x) > 0 then [swap (zPos - 1) zPos s] else [])
        ++ (if (mod zPos x) + 1 < x then [swap (zPos + 1) zPos s] else [])
        ++ (if (div zPos x) > 0 then [swap (zPos - x) zPos s] else [])
        ++ (if (div zPos x) + 1 < y then [swap (zPos + x) zPos s] else [])
        where   zPos = index 0 s

-- A wrapper for the elemIndex function
index :: Int -> [Int] -> Int
index a as = case elemIndex a as of
        Nothing -> -1
        Just i -> i

-- Swaps two items in a list
swap :: Int -> Int -> [Int] -> [Int]
swap a b nums = take low nums ++ [nums !! high] ++ drop (low + 1) (take high nums) ++ [nums !! low] ++ drop (high + 1) nums
        where   high = max a b
                low = min a b
