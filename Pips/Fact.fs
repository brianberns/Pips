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

module Operator =

    let apply pipValue op target =
        match op with
            | LessThan -> pipValue < target
            | LessThanOrEqualTo -> pipValue <= target
            | EqualTo -> pipValue = target
            | GreaterThan -> pipValue > target
            | GreaterThanOrEqualTo -> pipValue >= target

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
            | RegionType.SumLess target ->
                let op = LessThan
                {
                    Cell = cell
                    Operator = op
                    Target = target
                }
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

    let apply pipValue fact =
        Operator.apply pipValue fact.Operator fact.Target

type EdgeFact =
    | IntraRegionSum of
        {|
            CellA : Cell
            CellB : Cell
            Operator : Operator
            Target : int
        |}
    | InterRegion of CellFact * CellFact

module EdgeFact =

    let shrink region puzzle =
        let uncovered, covered =
            Array.partition
                (flip Puzzle.isEmpty puzzle)
                region.Cells
        match region.Type with
            | RegionType.SumLess target ->
                let sum =
                    Array.sumBy puzzle.Board.Item covered
                let target = target - sum
                assert(target >= PipCount.minValue * uncovered.Length)
                Array.singleton {
                    Cells = uncovered
                    Type = RegionType.SumLess target
                }
            | RegionType.SumExact target ->
                let sum =
                    Array.sumBy puzzle.Board.Item covered
                let target = target - sum
                assert(target >= PipCount.minValue * uncovered.Length)
                Array.singleton {
                    Cells = uncovered
                    Type = RegionType.SumExact target
                }
            | _ -> failwith "Unexpected"

    let getEdgeFacts (tiling : Tiling) puzzle =

        let regionMap =
            Map [
                for region in puzzle.Regions do
                    let regions = shrink region puzzle
                    for region in regions do
                        for cell in region.Cells do
                            cell, region
            ]

        [   // to-do: sort by "slack"
            for (cellA, cellB) in tiling do
                let regionA = regionMap[cellA]
                let regionB = regionMap[cellB]
                if regionA = regionB then   // to-do: region ID
                    match regionA.Type with
                        | RegionType.SumLess target ->
                            IntraRegionSum {|
                                CellA = cellA
                                CellB = cellB
                                Operator = LessThan
                                Target = target
                            |}
                        | RegionType.SumExact target ->
                            IntraRegionSum {|
                                CellA = cellA
                                CellB = cellB
                                Operator = LessThanOrEqualTo
                                Target = target
                            |}
                        | _ -> failwith "Unexpected"
                else
                    let factA = CellFact.create cellA regionA
                    let factB = CellFact.create cellB regionB
                    InterRegion (factA, factB)
        ]

    let apply domino edgeFact : seq<Edge> =
        seq {
            match edgeFact with
                | IntraRegionSum irs ->
                    let sum = domino.Left + domino.Right
                    if Operator.apply sum irs.Operator irs.Target then
                        irs.CellA, irs.CellB
                        if domino.Left <> domino.Right then
                            irs.CellB, irs.CellA
                | InterRegion (factA, factB) ->
                    if CellFact.apply domino.Left factA
                        && CellFact.apply domino.Right factB then
                        factA.Cell, factB.Cell
                    if domino.Left <> domino.Right
                        && CellFact.apply domino.Left factB
                        && CellFact.apply domino.Right factA then
                        factB.Cell, factA.Cell
        }

    let solveImpl tiling puzzle =

        let rec loop edgeFacts (tiling : Tiling) puzzle =
            match edgeFacts with
                | [] -> puzzle
                | edgeFact :: rest ->
                    let pairs =
                        puzzle.UnplacedDominoes
                            |> Seq.collect (fun domino ->
                                apply domino edgeFact
                                    |> Seq.map (fun edge ->
                                        domino, edge))
                            |> Seq.toArray
                    assert(pairs.Length > 0)
                    match Seq.tryExactlyOne pairs with
                        | Some (domino, edge) ->
                            let puzzle =
                                Puzzle.place domino edge puzzle
                            assert(
                                tiling.Contains(edge)
                                    || tiling.Contains(Edge.reverse edge))
                            let tiling =
                                tiling
                                    .Remove(edge)
                                    .Remove(Edge.reverse edge)
                            let edgeFacts = getEdgeFacts tiling puzzle
                            loop edgeFacts tiling puzzle
                        | None ->
                            loop rest tiling puzzle

        let edgeFacts = getEdgeFacts tiling puzzle
        loop edgeFacts tiling puzzle

    let solve puzzle =
        let tiling =
            Puzzle.getAllTilings puzzle
                |> Seq.exactlyOne   // to-do: fix
        solveImpl tiling puzzle
