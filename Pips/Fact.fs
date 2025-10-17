namespace Pips

type Operator =
    | LessThan
    | LessThanOrEqualTo
    | EqualTo
    | GreaterThan
    | GreaterThanOrEqualTo

    override op.ToString() =
        match op with
            | LessThan -> "<"
            | LessThanOrEqualTo -> "≤"
            | EqualTo -> "="
            | GreaterThan -> ">"
            | GreaterThanOrEqualTo -> "≥"

type Fact =
    {
        Cell : Cell
        Operator : Operator
        Target : int
    }

    override fact.ToString () =
        $"{fact.Cell} {fact.Operator} {fact.Target}"

module Fact =

    let private create cell region =
        match region.Type with
            | RegionType.SumExact target ->
                let op =
                    if region.Cells.Length = 1 then
                        EqualTo
                    else
                        LessThanOrEqualTo
                {
                    Cell = cell
                    Operator = op
                    Target = target
                }
            | _ -> failwith "Unexpected"

    let private createRegionFacts (tiling : Tiling) region =
        seq {
            for edge in tiling do
                for cell in region.Cells do
                    if Edge.contains cell edge then
                        create cell region
        }

    let createPuzzleFacts tiling puzzle =
        seq {
            for region in puzzle.Regions do
                yield! createRegionFacts tiling region
        }
