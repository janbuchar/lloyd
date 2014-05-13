import qualified Data.Set as Set
import Data.List (elemIndex)

data State = State {x :: Int, y :: Int, board :: [Int]} deriving (Eq)

instance Ord State where
        compare s1 s2 = compare (price s1) (price s2)

-- Takes the dimensions of the board and a list describing the board.
-- Returns a list of fields to swap with the zero field
solve :: Int -> Int -> [Int] -> Maybe [Int]
solve x y board 
        | length board /= x * y = Nothing
        | not (elem 0 board) = Nothing
        | otherwise = search (Set.singleton (State x y board)) Set.empty

-- Perform an A* search
search :: Set.Set State -> Set.Set [Int] -> Maybe [Int]
search queue seen
        | Set.null queue = Nothing
        | s == (goalState x y) = Just [0] -- Placeholder
        | otherwise = search (Set.union queue' next) (Set.insert b seen)
        where   (s@(State x y b), queue') = Set.deleteFindMin queue
                succ = filter (\a -> not (Set.member a seen)) (nextStates s)
                next = Set.fromList $ map (\a -> (State x y a)) succ

-- The goal state for given dimensions
goalState :: Int -> Int -> State
goalState x y = (State x y ([1..x * y - 1] ++ [0]))

-- Generate state transitions for given state
nextStates :: State -> [[Int]]
nextStates (State x y s) = []
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

-- Calculates the distance of given state from the goal state
-- The distance is defined as the sum of Manhattan distances
-- of the blocks from their positions in the goal state
price :: State -> Int
price (State x y s) = foldl (+) 0 $ map (blockDistance x y) (zip positions goal)
        where   positions = [0 .. x * y - 1]
                goal = map (\n -> if n == 0 then x * y - 1 else n - 1) s

-- Calculates the distance of two blocks in a square grid
blockDistance :: Int -> Int -> (Int, Int) -> Int
blockDistance x y (a, b) = dx + dy
        where   dx = abs ((mod a x) - (mod b x))
                dy = abs ((div a x) - (div b x))
