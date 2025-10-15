namespace Pips

open Xunit

module TilingTree =

    let private createCells pairs =
        pairs
            |> Seq.map (fun (row, col) ->
                Cell.create row col)
            |> set

    let private count tilingTrees =
        let rec loop (Node (_, children)) =
            if children.Length = 0 then 1
            else Array.sumBy loop children
        Array.sumBy loop tilingTrees

    [<Fact>]
    let ``1x1 grid`` () =
        let cells =
            createCells [(0, 0)]
        let tilingTrees = TilingTree.getAll cells
        Assert.Equal(0, count tilingTrees)

    [<Fact>]
    let ``1x4 grid`` () =
        let cells =
            createCells [
                (0, 0); (0, 1); (0, 2); (0, 3)
            ]
        let tilingTrees = TilingTree.getAll cells
        Assert.Equal(1, count tilingTrees)

    [<Fact>]
    let ``2x2 grid`` () =
        let cells =
            createCells [
                (0, 0); (0, 1)
                (1, 0); (1, 1)
            ]
        let tilingTrees = TilingTree.getAll cells
        Assert.Equal(2, count tilingTrees)

    [<Fact>]
    let ``2x3 grid`` () =
        let cells =
            createCells [
                (0, 0); (0, 1); (0, 2)
                (1, 0); (1, 1); (1, 2)
            ]
        let tilingTrees = TilingTree.getAll cells
        Assert.Equal(3, count tilingTrees)

    [<Fact>]
    let ``L-shape horizontal`` () =
        let cells =
            createCells [
                (0, 0); (0, 1); (0, 2)
                (1, 0)
            ]
        let tilingTrees = TilingTree.getAll cells
        Assert.Equal(1, count tilingTrees)

    [<Fact>]
    let ``L-shape vertical`` () =
        let cells =
            createCells [
                (0, 0); (0, 1)
                (1, 0)
                (2, 0)
            ]
        let tilingTrees = TilingTree.getAll cells
        Assert.Equal(1, count tilingTrees)

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
        let tilingTrees = TilingTree.getAll cells
        Assert.Equal(1, count tilingTrees)
