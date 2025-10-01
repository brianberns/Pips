namespace Pips

type Constraint =
    | Unconstrained
    | Equal
    | Unequal
    | SumLess of int
    | SumGreater of int
    | Sum of int

type Region =
    {
        Cells : Cell[]
        Type : Constraint
    }

module Region =

    let getValues board region =
        region.Cells
            |> Array.choose (fun cell ->
                Board.tryGetValue cell board)

    let tryGetValues board region =
        let values = getValues board region
        if values.Length = region.Cells.Length then
            Some values
        else
            None

    let isSolved board region =
        match tryGetValues board region, region.Type with
            | None, _ -> false
            | Some _, Unconstrained -> true
            | Some values, Equal ->
                (Array.distinct values).Length = 1
            | Some values, Unequal ->
                (Array.distinct values).Length = values.Length
            | Some values, SumLess n ->
                Array.sum values < n
            | Some values, SumGreater n ->
                Array.sum values > n
            | Some values, Sum n ->
                Array.sum values = n
