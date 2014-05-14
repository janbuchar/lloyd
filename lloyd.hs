import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.List (elemIndex, find)
import Debug.Trace

data State = State {x :: Int, y :: Int, steps :: Int, board :: [Int]}

instance Ord State where
        compare s1 s2 = compare (price s1) (price s2)

instance Eq State where
        (State x1 y1 _ b1) == (State x2 y2 _ b2) = and [x1 == x2, y1 == y2, b1 == b2]

instance Show State where
        show (State _ _ _ b) = show b

-- Takes the dimensions of the board and a list describing the board.
-- Returns a list of fields to swap with the zero field
solve :: Int -> Int -> [Int] -> Maybe [Int]
solve x y board 
        | length board /= x * y = Nothing
        | not (elem 0 board) = Nothing
        | otherwise = search (Set.singleton (State x y 0 board)) Map.empty

-- Perform an A* search
search :: Set.Set State -> Map.Map [Int] Int -> Maybe [Int]
search queue record
        | Set.null queue = Nothing
        | s == (goalState x y) = Just $ rebuildPath s record
        | otherwise = search (Set.union queue' next) (Map.insert b n record)
        where   (s@(State x y n b), queue') = Set.deleteFindMin queue
                next = Set.fromList $ filter (notVisited record) (nextStates s)

-- Return True iff given state hasn't been visited yet
notVisited :: Map.Map [Int] Int -> State -> Bool
notVisited v (State _ _ _ s) = Map.notMember s v

-- The goal state for given dimensions
goalState :: Int -> Int -> State
goalState x y = (State x y 0 ([1..x * y - 1] ++ [0]))

-- Generate state transitions for given state
nextStates :: State -> [State]
nextStates (State x y n s) = map (State x y (n + 1)) ([]
        ++ (if (mod zPos x) > 0 then [swap (zPos - 1) zPos s] else [])
        ++ (if (mod zPos x) + 1 < x then [swap (zPos + 1) zPos s] else [])
        ++ (if (div zPos x) > 0 then [swap (zPos - x) zPos s] else [])
        ++ (if (div zPos x) + 1 < y then [swap (zPos + x) zPos s] else []))
        where   zPos = index 0 s

-- Reconstruct the path to given state
rebuildPath :: State -> Map.Map [Int] Int -> [Int]
rebuildPath s record = reverse (rebuildPath' s record)

rebuildPath' :: State -> Map.Map [Int] Int -> [Int]
rebuildPath' s@(State x y n b) record
        | n == 0 = []
        | otherwise = (getMovedBlock s s') : (rebuildPath' s' record)
        where   bs = map (\(State _ _ _ b) -> b) (nextStates s)
                st = head $ filter (\b -> (Map.lookup b record) == Just (n - 1)) bs
                s' = (State x y (n - 1) st)

-- Get the block that was moved to the empty place
getMovedBlock :: State -> State -> Int
getMovedBlock (State _ _ _ b1) (State _ _ _ b2) = case res of
        Nothing -> undefined
        Just (a, b) -> a
        where res = find (\(a, b) -> a /= 0 && b == 0) (zip b1 b2)

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
price (State x y _ s) = foldl (+) 0 $ map (blockDistance x y) (zip positions goal)
        where   positions = [0 .. x * y - 1]
                goal = map (\n -> if n == 0 then x * y - 1 else n - 1) s

-- Calculates the distance of two blocks in a square grid
blockDistance :: Int -> Int -> (Int, Int) -> Int
blockDistance x y (a, b) = dx + dy
        where   dx = abs ((mod a x) - (mod b x))
                dy = abs ((div a x) - (div b x))
