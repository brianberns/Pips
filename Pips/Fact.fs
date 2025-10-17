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

type CellFact =
    {
        Cell : Cell
        Operator : Operator
        Target : int
    }

    override fact.ToString () =
        $"{fact.Cell} {fact.Operator} {fact.Target}"

module CellFact =

    let create cell region =
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

    let getStringency fact =
        min
            (fact.Target - PipCount.minValue)
            (PipCount.maxValue - fact.Target)

    let apply pipValue fact =
        match fact.Operator with
            | LessThan -> pipValue < fact.Target
            | LessThanOrEqualTo -> pipValue <= fact.Target
            | EqualTo -> pipValue = fact.Target
            | GreaterThan -> pipValue > fact.Target
            | GreaterThanOrEqualTo -> pipValue >= fact.Target

type EdgeFact = CellFact * CellFact

module EdgeFact =

    let createEdgeFacts (tiling : Tiling) puzzle =
        let regionMap =
            Map [
                for region in puzzle.Regions do
                    for cell in region.Cells do
                        cell, region
            ]
        [|
            for (cellA, cellB) in tiling do
                let regionA = regionMap[cellA]
                let regionB = regionMap[cellB]
                (CellFact.create cellA regionA,
                 CellFact.create cellB regionB) : EdgeFact
        |]

    let getStringency ((factA, factB) : EdgeFact) =
        CellFact.getStringency factA + CellFact.getStringency factB

    let tryApply domino ((factA, factB) : EdgeFact) =
        if CellFact.apply domino.Left factA
            && CellFact.apply domino.Right factB then
            Some (factA.Cell, factB.Cell)
        elif CellFact.apply domino.Left factB
            && CellFact.apply domino.Right factA then
            Some (factB.Cell, factA.Cell)
        else None

    let private applyEdgeFact edgeFact (dominoes : Set<Domino>) =
        

    let rec apply (edgeFacts : seq<EdgeFact>) (dominoes : Set<Domino>) =
        let edgeFacts =
            edgeFacts
                |> Seq.sortBy getStringency
                |> Seq.toList
        match edgeFacts with
            | [] -> []
            | edgeFact :: rest ->
                