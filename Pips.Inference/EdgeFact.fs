namespace Pips

type EdgeFact =

    | IntraRegionUnconstrained of
        {|
            CellA : Cell
            CellB : Cell
        |}

    | IntraRegionEquality of
        {|
            CellA : Cell
            CellB : Cell
        |}

    | IntraRegionInequality of
        {|
            CellA : Cell
            CellB : Cell
        |}

    | IntraRegionSum of
        {|
            CellA : Cell
            CellB : Cell
            Operator : ComparisonOperator
            Target : int
        |}

    | InterRegion of CellFact * CellFact

module EdgeFact =

    let shrink (region : Region) puzzle =

        let uncovered, covered =
            Array.partition
                (flip Puzzle.isEmpty puzzle)
                region.Cells
        let values = Array.map puzzle.Board.Item covered

        match region.Type with

            | RegionType.Equal when covered.Length > 0 ->
                let target =
                    Seq.distinct values |> Seq.exactlyOne
                uncovered
                    |> Array.map (fun cell ->
                        {
                            Cells = [| cell |]
                            Type = RegionType.SumExact target
                        })

            | RegionType.SumLess target ->
                let sum = Array.sum values
                let target = target - sum
                assert(target >= PipCount.minValue * uncovered.Length)
                Array.singleton {
                    Cells = uncovered
                    Type = RegionType.SumLess target
                }

            | RegionType.SumExact target ->
                let sum = Array.sum values
                let target = target - sum
                assert(target >= PipCount.minValue * uncovered.Length)
                Array.singleton {
                    Cells = uncovered
                    Type = RegionType.SumExact target
                }

            | _ -> Array.singleton region

    let getEdgeFacts (tiling : Tiling) puzzle =

        let regionMap =
            Map [
                for region in puzzle.Regions do
                    let regions = shrink region puzzle
                    for region in regions do
                        for cell in region.Cells do
                            cell, region
            ]

        [|   // to-do: sort by "slack"
            for (cellA, cellB) in tiling do
                let regionA = regionMap[cellA]
                let regionB = regionMap[cellB]
                if regionA = regionB then   // to-do: region ID
                    match regionA.Type with

                        | RegionType.Any ->
                            IntraRegionUnconstrained {|
                                CellA = cellA
                                CellB = cellB
                            |}

                        | RegionType.Equal ->
                            IntraRegionEquality {|
                                CellA = cellA
                                CellB = cellB
                            |}

                        | RegionType.Unequal ->
                            IntraRegionInequality {|
                                CellA = cellA
                                CellB = cellB
                            |}

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
        |]

    let apply domino edgeFact : seq<Edge> =
        seq {
            match edgeFact with

                | IntraRegionUnconstrained iru ->
                    iru.CellA, iru.CellB

                | IntraRegionEquality ire ->
                    if domino.Left = domino.Right then
                        ire.CellA, ire.CellB

                | IntraRegionInequality iri ->
                    if domino.Left <> domino.Right then
                        iri.CellA, iri.CellB
                        iri.CellB, iri.CellA

                | IntraRegionSum irs ->
                    let sum = domino.Left + domino.Right
                    if Operator.compare sum irs.Operator irs.Target then
                        irs.CellA, irs.CellB
                        if domino.Left <> domino.Right then
                            irs.CellB, irs.CellA

                | InterRegion (factA, factB) ->
                    if CellFact.compare domino.Left factA
                        && CellFact.compare domino.Right factB then
                        factA.Cell, factB.Cell
                    if domino.Left <> domino.Right
                        && CellFact.compare domino.Left factB
                        && CellFact.compare domino.Right factA then
                        factB.Cell, factA.Cell
        }
