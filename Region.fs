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

    let getValues board region =
        region.Cells
            |> Array.choose (fun cell ->
                Board.tryGetValue cell board)

    let isSolved board region =
        let values = getValues board region
        if values.Length = region.Cells.Length then
            match region.Type with
                | RegionType.Unconstrained -> true
                | RegionType.Equal ->
                    (Array.distinct values).Length = 1
                | RegionType.Unequal ->
                    (Array.distinct values).Length = values.Length
                | RegionType.SumLess n ->
                    Array.sum values < n
                | RegionType.SumGreater n ->
                    Array.sum values > n
                | RegionType.Sum n ->
                    Array.sum values = n
        else false
