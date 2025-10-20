namespace Pips

type CellFact =
    | Unconstrained of Cell
    | Comparison of
        {|
            Cell : Cell
            Operator : ComparisonOperator
            Target : int
        |}

    override fact.ToString () =
        match fact with
            | Unconstrained cell ->
                $"{cell} = *"
            | Comparison comp ->
                $"{comp.Cell} {comp.Operator} {comp.Target}"

    member fact.Cell =
        match fact with
            | Unconstrained cell -> cell
            | Comparison comp -> comp.Cell

module CellFact =

    let create cell region =

        match region.Type with

            | RegionType.SumLess target ->
                let op = LessThan
                Comparison {|
                    Cell = cell
                    Operator = op
                    Target = target
                |}

            | RegionType.SumExact target ->
                let op =
                    if region.Cells.Length = 1 then
                        EqualTo
                    else
                        LessThanOrEqualTo
                Comparison {|
                    Cell = cell
                    Operator = op
                    Target = target
                |}

            | _ -> Unconstrained cell

    let apply pipValue = function
        | Unconstrained _ -> true
        | Comparison comp ->
            Operator.apply pipValue comp.Operator comp.Target
