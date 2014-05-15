lloyd
=====

A solver for Lloyd's fifteen-like puzzles

Input
-----
The program takes the dimensions of the board on the first input line and the board 
itself (by rows) on the second line, where 0 represents the empty place.

Example input
----
Solve for a two by two board where the first row is (0, 1) and the second row is (3, 2).
	2 2
	0 1 3 2

Output
-----
The program outputs the numbers of blocks that should be moved to the empty place to solve the puzzle.

Example output
----
Solution for the example input - first move the "1" block, then move the "2" block.
	1
	2

Implementation details
-----
The program uses the A-star search algorithm to find a solution (search function). 
It starts with the initial state of the board, expands it (nextStates function), puts 
the new states in a priority queue (a Set, which is guaranteed to be ordered), and 
then continues recursively with the top item in the queue.

The states in the queue are sorted by a simple heuristic function (price function).
This function calculates the sum of Manhattan distances between the current positions
of the blocks and their goal positions.

The search function also maintains a Map of visited states along with the number of
steps required to reach it. This prevents the algorithm from getting stuck in an
infinite loop.

When the search finds a path to the goal state, it has to reconstruct it (rebuildPath
function). For a state S reachable in n steps, rebuildPath searches the Map for a 
state that is one step (via nextStates) from S and is reachable in n - 1 steps. 
This new state is added to the path and then rebuildPath is applied recursively.
