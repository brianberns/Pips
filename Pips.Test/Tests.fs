namespace Pips

open Xunit

module Cell =

    let create row col =
        { Row = row; Column = col }

module Tiling =

    let private createCells pairs =
        pairs
            |> Seq.map (fun (row, col) ->
                Cell.create row col)
            |> set

    let private count tilings =
        let rec loop (Node (_, _, children)) =
            if children.Length = 0 then 1
            else Array.sumBy loop children
        Array.sumBy loop tilings

    [<Fact>]
    let Example () =
        let cells =
            createCells [(0, 0); (0, 1); (1, 0); (1, 1)]
        let tilings = Tiling.getAll cells
        Assert.Equal(2, count tilings)

module Puzzle =

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
                    UnplacedDominoes = Set.empty
                    Board =
                        puzzle.Board
                            |> place dominoes[0] (0, 1) (0, 2)
                            |> place dominoes[1] (2, 2) (2, 1)
                            |> place dominoes[2] (1, 1) (1, 0)
                            |> place dominoes[3] (1, 2) (1, 3)
            }
        Assert.Equal(expected, Puzzle.solve puzzle)
