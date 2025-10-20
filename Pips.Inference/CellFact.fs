namespace Pips

/// A cell fact constrains the value of a cell.
type CellFact =

    /// Constrain by comparison to a target.
    | Comparison of
        {|
            Cell : Cell
            Operator : ComparisonOperator
            Target : int
        |}

    /// Unconstrained value.
    | Unconstrained of Cell

    /// Cell constrained by this fact.
    member fact.Cell =
        match fact with
            | Comparison comp -> comp.Cell
            | Unconstrained cell -> cell

    /// Display string.
    override fact.ToString () =
        match fact with
            | Comparison comp ->
                $"{comp.Cell} {comp.Operator} {comp.Target}"
            | Unconstrained cell ->
                $"{cell} = *"

module CellFact =

    /// Constrains the given cell within its region.
    let create cell (region : Region) =
        assert(Array.contains cell region.Cells)

        match region.Type with

                // cell value must be less than the region target,
                // even when other cells in region are empty
            | RegionType.SumLess target ->
                Comparison {|
                    Cell = cell
                    Operator = LessThan
                    Target = target
                |}

                // if this is the only cell in the region, its value
                // must be greater than the target
            | RegionType.SumGreater target
                when region.Cells.Length = 1 ->
                Comparison {|
                    Cell = cell
                    Operator = GreaterThan
                    Target = target
                |}

                // if this is the only cell in the region, its value
                // must match the target; otherwise, it may be less
                // than the target
            | RegionType.SumExact target ->
                let op =
                    if region.Cells.Length = 1 then
                        EqualTo
                    else LessThanOrEqualTo
                Comparison {|
                    Cell = cell
                    Operator = op
                    Target = target
                |}

                // we don't have enough information to the constrain
                // the cell value
            | _ -> Unconstrained cell

    /// Is the given pip value valid for this constraint?
    let isValid pipValue = function
        | Comparison comp ->
            ComparisonOperator.compare
                pipValue
                comp.Operator
                comp.Target
        | Unconstrained _ -> true
