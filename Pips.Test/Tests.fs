namespace Pips

open Xunit

module Test =

    let private place
        domino
        (leftRow, leftCol)
        (rightRow, rightCol)
        board =
        Board.place
            domino
            { Row = leftRow; Column = leftCol }
            { Row = rightRow; Column = rightCol }
            board

    let private puzzleMap = Daily.loadFile "2025-09-30.json"

    [<Fact>]
    let Easy () =
        let puzzle = puzzleMap["easy"]
        let dominoes = Seq.toArray puzzle.UnplacedDominoes
        let expected =
            Array.singleton {
                puzzle with
                    UnplacedDominoes = List.empty
                    Board =
                        puzzle.Board
                            |> place dominoes[0] (0, 1) (0, 2)
                            |> place dominoes[1] (2, 2) (2, 1)
                            |> place dominoes[2] (1, 1) (1, 0)
                            |> place dominoes[3] (1, 2) (1, 3)
            }
        Assert.Equal(expected, Puzzle.solve puzzle)
