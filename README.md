# Pips!

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

[Pips](https://www.nytimes.com/games/pips) is a game from the New York Times. The object is to cover an irregular board of square cells with a set of dominoes, subject to some constraints, such as that the number of pips in a region of the board must sum to a specific number. The Times publishes an easy, medium, and hard Pips puzzle every day.

Solving Pips is a good programming challenge because the number of possible solutions increases quickly as the board gets larger. Some of the hard-level Pips games can take a very long time to solve by a brute force search, so we'll have to be clever to get the time under, say, a few seconds in the worst case.

# Backtracking

The approach we'll use is called a [backtracking](https://en.wikipedia.org/wiki/Backtracking) algorithm, which is essentially a rigorous version of "trial and error". The idea is to place one domino at a time on the board. If the resulting state of the puzzle is still valid after placing a domino (i.e. conforms to all the constraints), then we repeat the procedure with another domino in another location. If the placement is invalid, we pick up that domino and try another one. In this way, we will eventually find all solutions, or we can stop after we find the first one.

We'll use [F#](https://fsharp.org/) because functional programming is a good choice for "black box" problems like this that have no side-effects, and [.NET](https://dotnet.microsoft.com/) is an easy, fast platform to work with.