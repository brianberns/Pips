namespace Pips

[<RequireQualifiedAccess>]
type RegionType =
    | Unconstrained
    | Equal
    | Unequal
    | SumLess of int
    | SumGreater of int
    | Sum of int

type Region =
    {
        Cells : Cell[]
        Type : RegionType
    }

module Region =

    let getPipCounts board region =
        region.Cells
            |> Array.choose (fun cell ->
                Board.tryGetPipCount cell board)

    let isValid board region =
        let pipCounts = getPipCounts board region
        match region.Type with
            | RegionType.Unconstrained -> true
            | RegionType.Equal ->
                (Array.distinct pipCounts).Length <= 1
            | RegionType.Unequal ->
                (Array.distinct pipCounts).Length = pipCounts.Length
            | RegionType.SumLess n ->
                assert(PipCount.minValue = 0)
                Array.sum pipCounts < n
            | RegionType.SumGreater n ->
                let nEmpty = region.Cells.Length - pipCounts.Length
                (Array.sum pipCounts) + (PipCount.maxValue * nEmpty) > n
            | RegionType.Sum n ->
                let sum = Array.sum pipCounts
                if pipCounts.Length = region.Cells.Length then
                    sum = n
                else
                    assert(PipCount.minValue = 0)
                    sum <= n

    let isSolved board region =
        let pipCounts = getPipCounts board region
        if pipCounts.Length = region.Cells.Length then
            match region.Type with
                | RegionType.Unconstrained -> true
                | RegionType.Equal ->
                    (Array.distinct pipCounts).Length = 1
                | RegionType.Unequal ->
                    (Array.distinct pipCounts).Length = pipCounts.Length
                | RegionType.SumLess n ->
                    Array.sum pipCounts < n
                | RegionType.SumGreater n ->
                    Array.sum pipCounts > n
                | RegionType.Sum n ->
                    Array.sum pipCounts = n
        else false
