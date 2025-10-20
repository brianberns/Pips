namespace Pips

/// An edge fact constrains the values of adjacent cells.
/// The cells may be in the same region, or in two different
/// regions.
type EdgeFact =

    /// Edge values must be equal.
    | SameRegionEqual of Edge

    /// Edge values must be unequal.
    | SameRegionUnequal of Edge   // to-do: and also unequal to other cells in the region

    /// Edge values are unconstrained.
    | SameRegionUnconstrained of Edge

    /// Sum of edge values is constrained.
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
        assert(
            Seq.forall (fun (cellA, cellB) ->
                Puzzle.isEmpty cellA puzzle
                    && Puzzle.isEmpty cellB puzzle)
                edges)

            // allow regions to be indexed
        let regions =
            [|
                for region in puzzle.Regions do
                    yield! Region.tighten puzzle.Board region
            |]
        let regionMap = Puzzle.getRegionMap regions

            // create a fact for each edge
        [|
            for ((cellA, cellB) as edge) in edges do

                    // cells are in the same region?
                let regionIdA = regionMap[cellA]
                let regionIdB = regionMap[cellB]
                if regionIdA = regionIdB then
                    match regions[regionIdA].Type with

                            // edge values must be equal to each other
                        | RegionType.Equal ->
                            SameRegionEqual edge

                            // edge values must not be equal to each other
                        | RegionType.Unequal ->
                            SameRegionUnequal edge

                            // edge values are unconstrained
                        | RegionType.Any ->
                            SameRegionUnconstrained edge

                            // sum of edge values must be less than overall
                            // region target
                        | RegionType.SumLess target ->
                            SameRegionSum {|
                                Edge = edge
                                Operator = LessThan
                                Target = target
                            |}

                            // sum of edge values must be less than or equal
                            // to the overall region target
                        | RegionType.SumExact target ->
                            SameRegionSum {|
                                Edge = edge
                                Operator = LessThanOrEqualTo
                                Target = target
                            |}

                        | _ -> failwith "Unexpected"

                    // cells straddle two different regions
                else
                    let factA =
                        CellFact.create cellA regions[regionIdA]
                    let factB =
                        CellFact.create cellB regions[regionIdB]
                    CrossRegion (factA, factB)

        |]   // to-do: sort these from most to least promising

    /// Generates possible edge orientations for the given domino
    /// as constrained by the given fact.
    let apply domino fact =
        seq {
            match fact with

                    // only a "double" domino matches
                | SameRegionEqual edge ->
                    if domino.Left = domino.Right then
                        edge

                    // only a "double" domino does not match
                | SameRegionUnequal edge ->
                    if domino.Left <> domino.Right then
                        edge
                        Edge.reverse edge

                    // any domino matches
                | SameRegionUnconstrained edge ->
                    edge
                    if domino.Left <> domino.Right then
                        Edge.reverse edge

                    // sum of domino values matches?
                | SameRegionSum case ->
                    let sum = domino.Left + domino.Right
                    let isValid =
                        ComparisonOperator.compare
                            sum
                            case.Operator
                            case.Target
                    if isValid then
                        case.Edge
                        if domino.Left <> domino.Right then
                            Edge.reverse case.Edge

                    // independent cell constraints both match?
                | CrossRegion (factA, factB) ->

                    if CellFact.isValid domino.Left factA
                        && CellFact.isValid domino.Right factB then
                        factA.Cell, factB.Cell

                    if domino.Left <> domino.Right
                        && CellFact.isValid domino.Left factB
                        && CellFact.isValid domino.Right factA then
                        factB.Cell, factA.Cell
        }
