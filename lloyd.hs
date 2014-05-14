import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.List (elemIndex, find)
import Debug.Trace

data State = State {cost :: Int, steps :: Int, board :: [Int]}

instance Ord State where
	compare (State c1 _ _) (State c2 _ _) = compare c1 c2

instance Eq State where
	(State _ _ b1) == (State _ _ b2) = b1 == b2

data Board = Board {x :: Int, y :: Int}

-- Make a state from a Board, number of steps and board state
-- (Calculates price automatically)
mkState :: Board -> Int -> [Int] -> State
mkState board n s = (State (price board s) n s)

-- Takes the dimensions of the board and a list describing the board.
-- Returns a list of fields to swap with the zero field
solve :: Int -> Int -> [Int] -> Maybe [Int]
solve x y board 
	| length board /= x * y = Nothing
	| not (elem 0 board) = Nothing
	| otherwise = search (Board x y) (Set.singleton init) Map.empty
	where init = mkState (Board x y) 0 board

-- Perform an A* search
search :: Board -> Set.Set State -> Map.Map [Int] Int -> Maybe [Int]
search board@(Board x y) queue record
	| Set.null queue = Nothing
	| s == (goalState board) = Just $ rebuildPath board s record
	| otherwise = search board (Set.union queue' next) (Map.insert b n record)
	where	(s@(State _ n b), queue') = Set.deleteFindMin queue
		next = Set.fromList $ filter (notVisited record) (nextStates board s)

-- Return True iff given state hasn't been visited yet
notVisited :: Map.Map [Int] Int -> State -> Bool
notVisited v (State _ _ s) = Map.notMember s v

-- The goal state for given dimensions
goalState :: Board -> State
goalState (Board x y) = (State 0 0 ([1..x * y - 1] ++ [0]))

-- Generate state transitions for given state
nextStates :: Board -> State -> [State]
nextStates b@(Board x y) (State _ n s) = map (mkState b (n + 1)) ([]
	++ (if (mod zPos x) > 0 then [swap (zPos - 1) zPos s] else [])
	++ (if (mod zPos x) + 1 < x then [swap (zPos + 1) zPos s] else [])
	++ (if (div zPos x) > 0 then [swap (zPos - x) zPos s] else [])
	++ (if (div zPos x) + 1 < y then [swap (zPos + x) zPos s] else []))
	where	zPos = index 0 s

-- Reconstruct the path to given state
rebuildPath :: Board -> State -> Map.Map [Int] Int -> [Int]
rebuildPath b s record = reverse (rebuildPath' b s record)

rebuildPath' :: Board -> State -> Map.Map [Int] Int -> [Int]
rebuildPath' board@(Board x y) s@(State _ n b) record
	| n == 0 = []
	| otherwise = (getMovedBlock s s') : (rebuildPath' board s' record)
	where	bs = map (\(State _ _ b) -> b) (nextStates board s)
		st = head $ filter (\b -> (Map.lookup b record) == Just (n - 1)) bs
		s' = mkState board (n - 1) st

-- Get the block that was moved to the empty place
getMovedBlock :: State -> State -> Int
getMovedBlock (State _ _ b1) (State _ _ b2) = case res of
	Nothing -> undefined
	Just (a, b) -> a
	where	res = find (\(a, b) -> a /= 0 && b == 0) (zip b1 b2)

-- A wrapper for the elemIndex function
index :: Int -> [Int] -> Int
index a as = case elemIndex a as of
	Nothing -> -1
	Just i -> i

-- Swaps two items in a list
swap :: Int -> Int -> [Int] -> [Int]
swap a b nums = take low nums ++ [nums !! high] ++ drop (low + 1) (take high nums) ++ [nums !! low] ++ drop (high + 1) nums
	where	high = max a b
		low = min a b

-- Calculates the distance of given state from the goal state
-- The distance is defined as the sum of Manhattan distances
-- of the blocks from their positions in the goal state
price :: Board -> [Int] -> Int
price b@(Board x y) s = sum $ map (blockDistance b) (zip positions goal)
	where	positions = [0 .. x * y - 1]
		goal = map (\n -> if n == 0 then x * y - 1 else n - 1) s

-- Calculates the distance of two blocks in a square grid
blockDistance :: Board -> (Int, Int) -> Int
blockDistance (Board x y) (a, b) = dx + dy
	where	dx = abs ((mod a x) - (mod b x))
		dy = abs ((div a x) - (div b x))
