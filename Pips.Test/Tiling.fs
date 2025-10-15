namespace Pips

open Xunit

module Tiling =

    let private createCells pairs =
        pairs
            |> Seq.map (fun (row, col) ->
                Cell.create row col)
            |> set

    let private createEdge ((rowA, colA), (rowB, colB)) : Edge =
        Cell.create rowA colA, Cell.create rowB colB

    [<Fact>]
    let ``1x1 grid`` () =
        let cells =
            createCells [(0, 0)]
        let tilings = Tiling.getAll cells
        Assert.Equal(0, tilings.Length)

    [<Fact>]
    let ``1x4 grid`` () =
        let cells =
            createCells [
                (0, 0); (0, 1); (0, 2); (0, 3)
            ]
        let tilings = Tiling.getAll cells
        Assert.Equal(1, tilings.Length)

    [<Fact>]
    let ``2x2 grid`` () =
        let cells =
            createCells [
                (0, 0); (0, 1)
                (1, 0); (1, 1)
            ]
        let tilings = Tiling.getAll cells
        Assert.Equal(2, tilings.Length)

    [<Fact>]
    let ``2x3 grid`` () =
        let cells =
            createCells [
                (0, 0); (0, 1); (0, 2)
                (1, 0); (1, 1); (1, 2)
            ]
        let tilings = Tiling.getAll cells
        Assert.Equal(3, tilings.Length)

    [<Fact>]
    let ``L-shape horizontal`` () =
        let cells =
            createCells [
                (0, 0); (0, 1); (0, 2)
                (1, 0)
            ]
        let tilings = Tiling.getAll cells
        Assert.Equal(1, tilings.Length)

    [<Fact>]
    let ``L-shape vertical`` () =
        let cells =
            createCells [
                (0, 0); (0, 1)
                (1, 0)
                (2, 0)
            ]
        let tilings = Tiling.getAll cells
        let expected =
            set [
                createEdge ((0, 0), (0, 1))
                createEdge ((1, 0), (2, 0))
            ]
        Assert.Equal<Tiling>([expected], tilings)

    [<Fact>]
    let Snake () =
        let cells =
            createCells [
                                        (0, 3);
                                        (1, 3);
                        (2, 1); (2, 2); (2, 3);
                        (3, 1);
                (4, 0); (4, 1)
            ]
        let tilings = Tiling.getAll cells
        Assert.Equal(1, tilings.Length)
