namespace Pips

open Xunit

module Puzzle =

    let private place
        (leftPipCount, rightPipCount)
        (leftRow, leftCol)
        (rightRow, rightCol)
        board =
        Board.place
            (Domino.create leftPipCount rightPipCount)
            (Cell.create leftRow leftCol)
            (Cell.create rightRow rightCol)
            board

    let private puzzleMap = Daily.loadFile "2025-09-30.json"

    [<Fact>]
    let ``2025-09-30-Easy`` () =
        let puzzle = puzzleMap["easy"]
        let solution =
            {
                puzzle with
                    UnplacedDominoes = Set.empty
                    Board =
                        puzzle.Board
                            |> place (4, 4) (0, 1) (0, 2)
                            |> place (3, 5) (2, 2) (2, 1)
                            |> place (0, 3) (1, 1) (1, 0)
                            |> place (2, 2) (1, 2) (1, 3)
            }

        let actual = Puzzle.solve puzzle
        Assert.Equal<List<_>>([solution], actual)

        let actual = Puzzle.trySolve puzzle
        Assert.Equal(Some solution, actual)
