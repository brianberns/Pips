namespace Pips

open Xunit

module Tiling =

    let private createCells pairs =
        pairs
            |> Seq.map (fun (row, col) ->
                Cell.create row col)
            |> set

    let private count tilings =
        let rec loop (Node (_, children)) =
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
    let ``1x4 grid`` () =
        let cells =
            createCells [(0, 0); (0, 1); (0, 2); (0, 3)]
        let tilings = Tiling.getAll cells
        Assert.Equal(1, count tilings)

    [<Fact>]
    let ``2x2 grid`` () =
        let cells =
            createCells [(0, 0); (0, 1); (1, 0); (1, 1)]
        let tilings = Tiling.getAll cells
        Assert.Equal(2, count tilings)

    [<Fact>]
    let ``L-shape`` () =
        let cells =
            createCells [(0, 0); (0, 1); (0, 2); (1, 0)]
        let tilings = Tiling.getAll cells
        Assert.Equal(1, count tilings)

    [<Fact>]
    let ``2025-10-18-Easy`` () =
        let cells =
            createCells [(0,3); (1,3); (2,3); (2,2); (2,1); (3,1); (4,1); (4,0)]
        let tilings = Tiling.getAll cells
        Assert.Equal(1, count tilings)
