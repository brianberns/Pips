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
            (Cell.create leftRow leftCol,
             Cell.create rightRow rightCol)
            board

    [<Fact>]
    let ``Easy puzzle from 2025-09-30`` () =
        let puzzleMap = Daily.loadFile "2025-09-30.json"
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

        let actual = Backtrack.solve puzzle
        Assert.Equal([solution], actual)

        let actual = Backtrack.trySolve puzzle
        Assert.Equal(Some solution, actual)

    [<Fact>]
    let ``Small puzzle with few solutions`` () =
        let puzzle =
            Puzzle.create
                [
                    Domino.create 1 3
                    Domino.create 5 5
                ]
                [|
                    {
                        Cells =
                            [|
                                Cell.create 0 0
                                Cell.create 0 1
                            |]
                        Type = RegionType.SumGreater 7
                    }
                    {
                        Cells =
                            [|
                                Cell.create 1 0
                                Cell.create 1 1
                            |]
                        Type = RegionType.SumExact 6
                    }
                |]
        let actual = Backtrack.solve puzzle
        Assert.Equal(2, Seq.length actual)

    [<Fact>]
    let ``Small puzzle with many solutions`` () =
        let puzzle =
            Puzzle.create
                [
                    Domino.create 0 2
                    Domino.create 4 3
                ]
                [|
                    {
                        Cells =
                            [|
                                Cell.create 1 1
                                Cell.create 0 1
                                Cell.create 1 2
                                Cell.create 0 2
                            |]
                        Type = RegionType.Unequal
                    }
                |]
        let actual = Backtrack.solve puzzle
        Assert.Equal(16, Seq.length actual)
