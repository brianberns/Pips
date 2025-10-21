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

[Pips](https://www.nytimes.com/games/pips) is a game from the New York Times. The object is to cover an irregular board of square cells with a set of dominoes, subject to some constraints. The Times publishes an easy, medium, and hard Pips puzzle every day.

Solving Pips is a good programming challenge because the number of possible solutions increases quickly as the board gets larger. Some of the hard-level Pips games can take a very long time to solve by a brute force search, so we'll have to be a bit clever to get the time under, say, a few seconds in the worst case.