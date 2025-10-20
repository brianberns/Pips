namespace Pips

/// An edge fact constrains the values of adjacent cells.
/// The cells may be in the same region, or in two different
/// regions.
type EdgeFact =

    /// Edge values must be equal.
    | SameRegionEqual of Edge

    /// Edge values must be unequal.
    | SameRegionUnequal of Edge

    /// Edge values are unconstrained.
    | SameRegionUnconstrained of Edge

    /// Edge value sum comparison.
    | SameRegionSum of
        {|
            Edge : Edge
            Operator : ComparisonOperator
            Target : int
        |}

    /// Edge straddles two regions, so it has two separate
    /// cell constraints.
    | CrossRegion of CellFact * CellFact

    /// Edge constrained by this fact.
    member fact.Edge =
        match fact with
            | SameRegionEqual edge
            | SameRegionUnequal edge
            | SameRegionUnconstrained edge -> edge
            | SameRegionSum case -> case.Edge
            | CrossRegion (factA, factB) ->
                factA.Cell, factB.Cell

module EdgeFact =

    /// Gets constraints for the given edges in the given puzzle.
    let getAll puzzle edges =

        let regionMap =
            Map [
                for region in puzzle.Regions do
                    let regions =
                        Region.tighten puzzle.Board region
                    for region in regions do
                        for cell in region.Cells do
                            cell, region
            ]

        [|   // to-do: sort by "slack"
            for (cellA, cellB) in edges do
                let regionA = regionMap[cellA]
                let regionB = regionMap[cellB]
                if regionA = regionB then   // to-do: region ID
                    match regionA.Type with

                        | RegionType.Any ->
                            SameRegionUnconstrained (cellA, cellB)

                        | RegionType.Equal ->
                            SameRegionEqual (cellA, cellB)

                        | RegionType.Unequal ->
                            SameRegionUnequal (cellA, cellB)

                        | RegionType.SumLess target ->
                            SameRegionSum {|
                                Edge = cellA, cellB
                                Operator = LessThan
                                Target = target
                            |}

                        | RegionType.SumExact target ->
                            SameRegionSum {|
                                Edge = cellA, cellB
                                Operator = LessThanOrEqualTo
                                Target = target
                            |}

                        | _ -> failwith "Unexpected"
                else
                    let factA = CellFact.create cellA regionA
                    let factB = CellFact.create cellB regionB
                    CrossRegion (factA, factB)
        |]

    let apply domino edgeFact : seq<Edge> =
        seq {
            match edgeFact with

                | SameRegionEqual edge ->
                    if domino.Left = domino.Right then
                        edge

                | SameRegionUnequal edge ->
                    if domino.Left <> domino.Right then
                        edge
                        Edge.reverse edge

                | SameRegionUnconstrained edge ->
                    edge
                    if domino.Left <> domino.Right then
                        Edge.reverse edge

                | SameRegionSum case ->
                    let sum = domino.Left + domino.Right
                    if ComparisonOperator.compare sum case.Operator case.Target then
                        case.Edge
                        if domino.Left <> domino.Right then
                            Edge.reverse case.Edge

                | CrossRegion (factA, factB) ->
                    if CellFact.isValid domino.Left factA
                        && CellFact.isValid domino.Right factB then
                        factA.Cell, factB.Cell
                    if domino.Left <> domino.Right
                        && CellFact.isValid domino.Left factB
                        && CellFact.isValid domino.Right factA then
                        factB.Cell, factA.Cell
        }
