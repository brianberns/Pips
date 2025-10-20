namespace Pips

/// An edge fact constrains the values of adjacent cells.
/// The cells may be in the same region, or in two different
/// regions.
type EdgeFact =

    /// Edge values must be equal.
    | SameRegionEquality of Edge

    /// Edge values must be unequal.
    | SameRegionInequality of Edge

    /// Edge values are unconstrained.
    | SameRegionUnconstrained of Edge

    /// Edge value sum comparison.
    | SameRegionSum of
        {|
            Edge : Edge
            Operator : ComparisonOperator
            Target : int
        |}

    /// Edge straddles two regions, so it's constrained by
    /// two independent cell constraints.
    | CrossRegion of CellFact * CellFact

    /// Edge constrained by this fact.
    member fact.Edge =
        match fact with
            | SameRegionEquality edge
            | SameRegionInequality edge
            | SameRegionUnconstrained edge -> edge
            | SameRegionSum case -> case.Edge
            | CrossRegion (factA, factB) ->
                factA.Cell, factB.Cell

module EdgeFact =

    let getEdgeFacts (tiling : Tiling) puzzle =

        let regionMap =
            Map [
                for region in puzzle.Regions do
                    let regions = Region.tighten puzzle region
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
                            SameRegionUnconstrained (cellA, cellB)

                        | RegionType.Equal ->
                            SameRegionEquality (cellA, cellB)

                        | RegionType.Unequal ->
                            SameRegionInequality (cellA, cellB)

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

                | SameRegionEquality edge ->
                    if domino.Left = domino.Right then
                        edge

                | SameRegionInequality edge ->
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
