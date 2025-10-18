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

    let getSlack fact =
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

    let getSlack ((factA, factB) : EdgeFact) =
        CellFact.getSlack factA + CellFact.getSlack factB

    let getEdgeFacts tiling puzzle =

        let regionMap =
            Map [
                for region in puzzle.Regions do
                    for cell in region.Cells do
                        cell, region
            ]

        List.sortBy getSlack [
            for (cellA, cellB) in tiling do
                let regionA = regionMap[cellA]
                let regionB = regionMap[cellB]
                CellFact.create cellA regionA,
                    CellFact.create cellB regionB
        ]

    let tryApply domino ((factA, factB) : EdgeFact) : Option<Edge> =
        if CellFact.apply domino.Left factA
            && CellFact.apply domino.Right factB then
            Some (factA.Cell, factB.Cell)
        elif CellFact.apply domino.Left factB
            && CellFact.apply domino.Right factA then
            Some (factB.Cell, factA.Cell)
        else None

    let rec solveImpl edgeFacts (dominoes : Set<Domino>) =
        match edgeFacts with
            | [] -> Array.empty
            | edgeFact :: rest ->
                let pairs =
                    dominoes
                        |> Seq.choose (fun domino ->
                            tryApply domino edgeFact
                                |> Option.map (fun edge ->
                                    domino, edge))
                        |> Seq.toArray
                assert(pairs.Length > 0)
                match Seq.tryExactlyOne pairs with
                    | Some (domino, edge) ->
                        [|
                            yield domino, edge
                            yield! solveImpl rest (dominoes.Remove(domino))
                        |]
                    | None ->
                        solveImpl rest dominoes

    let solve puzzle =
        let tiling =
            Puzzle.getAllTilings puzzle
                |> Seq.exactlyOne   // to-do: fix
        let edgeFacts = getEdgeFacts tiling puzzle
        solveImpl edgeFacts puzzle.UnplacedDominoes
