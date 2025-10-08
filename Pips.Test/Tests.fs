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
    let ``1x1 grid`` () =
        let cells =
            createCells [(0, 0)]
        let tilings = Tiling.getAll cells
        Assert.Equal(0, count tilings)

    [<Fact>]
    let ``2x2 grid`` () =
        let cells =
            createCells [(0, 0); (0, 1); (1, 0); (1, 1)]
        let tilings = Tiling.getAll cells
        Assert.Equal(2, count tilings)

    [<Fact>]
    let ``2025-10-18-Easy`` () =
        let cells =
            createCells [(0,3); (1,3); (2,3); (2,2); (2,1); (3,1); (4,1); (4,0)]
        let tilings = Tiling.getAll cells
        Assert.Equal(1, count tilings)

module Assert =

    let Equal<'t>(expected : seq<'t>, actual : seq<'t>) =
        Assert.Equal<seq<'t>>(expected, actual)

module Puzzle =

    let private place
        (leftPipCount, rightPipCount)
        (leftRow, leftCol)
        (rightRow, rightCol)
        board =
        Board.place
            { Left = leftPipCount; Right = rightPipCount }
            (Cell.create leftRow leftCol)
            (Cell.create rightRow rightCol)
            board

    let private puzzleMap = Daily.loadFile "2025-09-30.json"

    [<Fact>]
    let ``2025-09-30-Easy`` () =
        let puzzle = puzzleMap["easy"]
        let expected =
            List.singleton {
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
        Assert.Equal(expected, actual)
