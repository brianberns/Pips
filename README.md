# Solving Pips with F#

```
        ┌───────┐
        │     = │
    ┌───┴───┬───┴───┬───┬───┬───┐
    │     6 │    12 │ 5 │   │   │
    ├───┬───┼───┬───┴───┤   │   ├───┐
    │   │   │ * │     ≠ │   │10 │ * │
┌───┘   │   ├───┼───────┤   ├───┴───┤
│    18 │   │ * │    10 │ 6 │       │
└───────┘   ├───┴───┬───┴───┤       │
            │       │       │     0 │
            │       │       ├───┬───┤
            │     4 │       │<3 │<2 │
            └───────┘       └───┴───┘

Dominoes:

0-1   0-2   1-1   1-2   1-5   2-4   3-0
3-4   3-6   4-0   4-5   5-2   6-2   6-4
6-5   6-6

Found 1 solution(s) in 00:00:02.5545949:

        ┌───┬───┐
        │ 4 │ 4 │
    ┌───┤   │   ├───────┬───────┐
    │ 3 │ 3 │ 6 │ 6   5 │ 2   6 │
    │   ├───┼───┴───┬───┴───┬───┼───┐
    │ 6 │   │ 4   5 │ 4   2 │ 4 │ 3 │
┌───┴───┤   ├───┬───┼───────┤   │   │
│ 6   6 │   │ 2 │ 5 │ 5   2 │ 0 │ 0 │
└───────┘   │   │   ├───────┼───┼───┤
            │ 1 │ 1 │       │ 0 │ 0 │
            ├───┴───┤       │   │   │
            │ 1   1 │       │ 2 │ 1 │
            └───────┘       └───┴───┘
```

# Introduction

[Pips](https://www.nytimes.com/games/pips) is a game from the New York Times. The object is to cover an irregular board of square cells with a set of dominoes, subject to some constraints, such as that the number of pips in a region of the board must sum to a specific number. The Times publishes an easy, medium, and hard Pips puzzle every day. (In fact, as of this writing, they publish the puzzle data well ahead of time, if you're willing to read [JSON](https://www.nytimes.com/svc/pips/v1/2025-11-13.json).)

Solving Pips is a good programming challenge because the number of possible solutions increases quickly as the board gets larger. Some of the hard-level Pips games can take a very long time to solve by a brute force search, so we'll have to be clever to get the time under, say, a few seconds in the worst case.

# Backtracking

The algorithm we'll use is called  [backtracking](https://en.wikipedia.org/wiki/Backtracking), which is essentially a rigorous version of "trial and error". The idea is to place one domino at a time on the board. If, after placing a domino, the resulting state of the puzzle is still valid (i.e. conforms to all the constraints), then we repeat the procedure with another domino in another location, etc. If the placement is invalid, we pick up that domino and try another one in that spot. In this way, we will eventually find all solutions to the puzzle, or we can stop after we find the first one. In practice, most of the hard Pips puzzles have a single solution, but a few have more than 100 distinct solutions, and one has [2,764,800](https://www.forbes.com/sites/erikkain/2025/09/14/todays-nyt-pips-hints-and-solutions-for-monday-september-15th/)!

We'll use [F#](https://fsharp.org/) to implement this algorithm because functional programming is a good choice for "black box" problems like this that have no side-effects, and [.NET](https://dotnet.microsoft.com/) is an easy, fast platform to work with. (F# is actually a great all-purpose language for just about anything, but I digress.)

To make the search for solutions tenable, we'll use two enhancements over vanilla backtracking:

* Use geometric information about possible tilings to guide the search.
* Prune the search tree aggressively to avoid investigating dead ends.

More on both of these below.

# Tiling

One key observation is that there are only so many ways to tile a given board with dominoes. For example, there are just three ways to tile a 2×3 rectangle:

```
┌───┬───┬───┐      ┌───┬───────┐      ┌───────┬───┐
│ ■ │ ■ │ ■ │      │ ■ │ ■   ■ │      │ ■   ■ │ ■ │
│   │   │   │      │   ├───────┤      ├───────┤   │
│ ■ │ ■ │ ■ │      │ ■ │ ■   ■ │      │ ■   ■ │ ■ │
└───┴───┴───┘      └───┴───────┘      └───────┴───┘
```

So a tiling that starts off like this:

```
┌───┬───────┐
│   │ ■   ■ │
├───┴───┬───┤
│ ■   ■ │   │
└───────┴───┘
```
 is bound to fail because we've left two unconnected 1x1 areas, and there's no way to tile an odd number of cells with dominoes.

 We can use this to reduce the number of board configurations we have to examine when searching for Pips solutions. For example, if we start by placing a domino horizontally in the top-left corner of the 2×3 board, we know where the other two dominoes have to go:

 ```
 ┌───────┬───┐     ┌───────┬───┐
 │ ■   ■ │   │     │ ■   ■ │ ■ │
 ├───────┘   │  →  ├───────┤   │ 
 │           │     │ ■   ■ │ ■ │
 └───────────┘     └───────┴───┘
 ```

 To guide our backtracking algorithm, we can organize the tilings for a board into a "forest" of trees. Each node in a tree shows the placement of a domino in the tiling, and its child nodes show how the rest of the dominoes are placed, until we get each of the complete tilings as leaf nodes. For example, here are the five distinct tilings of a 2x4 rectangle arranged step-by-step in trees:

 ![Tiling trees](Tiling.svg)

 Our backtracking algorithm is now:

 * Given: A Pips puzzle in some state of completion, and a collection of tiling trees that indicate where the next domino might be placed.
 * If there are no more dominoes to place, the puzzle is solved.
 * Otherwise, for each given tiling tree:
   * Get the next domino location from the root of the tree.
   * Try placing each unplaced domino in that location. If that is a valid placement, recursively apply the algorithm to the child trees. (Make sure to place the domino in both orientations, if it is not a "double".)

# Implementation

We're finally ready to turn these ideas into code!

## Domino

True to the name of the game, the dots on a domino are called "pips", and each side of a domino has between 0 and 6 pips. For example, this is the 6-5 domino:

```
┌───────┬───────┐
│ o o o │ o   o │
│       │   o   │
│ o o o │ o   o │
└───────┴───────┘
```

The corresponding F# types:

```fsharp
/// Number of pips on one side of a domino.
type PipCount = int

/// The two sides of a domino.
type Domino =
    {
        /// Left side of the domino.
        Left : PipCount

        /// Right side of the domino.
        Right : PipCount
    }
```

A domino is a "double" if the pip count is the same on both sides:

```fsharp
module Domino =

    /// Is the given domino a "double", such as 6-6?
    let isDouble domino =
        domino.Left = domino.Right
```

Doubles are special because they only have one distinct orientation, while other dominoes have two.

## Cell

Each cell on the board has a (row, column) coordinate:

```fsharp
/// A cell in a grid.
type Cell =
    {
        /// Row coordinate (0-based).
        Row : int

        /// Column coordinate (0-based).
        Column : int
    }
```

And in order to place dominoes correctly, we need to define what it means for two cells to be adjacent:

```fsharp
module Cell =

    /// Gets all possible cells adjacent to the given cell.
    /// Some of these cells might not actually exist, though.
    let getAdjacent cell =
        [|
            { cell with Row = cell.Row - 1 }
            { cell with Row = cell.Row + 1 }
            { cell with Column = cell.Column - 1 }
            { cell with Column = cell.Column + 1 }
        |]
```
