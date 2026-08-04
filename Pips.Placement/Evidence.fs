namespace Pips

/// Evidence of where a domino can and can't be placed.
type DominoEvidence =
    {
        /// Domino being placed.
        Domino : Domino

        /// Valid edges.
        ValidEdges : Edge[]

        /// Invalid edges grouped by region and (in)validation
        /// result.
        InvalidEdgesGrouped :
            ((Region * ValidationResult) * Edge[])[]
    }

module DominoEvidence =

    /// Explains where the given domino can and can't be placed
    /// in the given puzzle.
    let get domino puzzle =

            // get all edges on which this domino might be placed
        let edges =
            let forward =
                puzzle
                    |> Puzzle.getAllTilings
                    |> Set.unionMany
            if Domino.isDouble domino then
                forward
            else
                let reverse = Set.map Edge.reverse forward
                Set.union forward reverse

            // separate valid from invalid edges
        let validEdges, invalidEdgeArrays =
            edges
                |> Seq.toArray
                |> Array.partitionWith (fun edge ->
                    let result =
                        Puzzle.tryPlaceTiledValid domino edge puzzle
                    match result with
                        | Ok _ -> Choice1Of2 edge
                        | Error pairs -> Choice2Of2 (edge, pairs))
        let invalidEdgesFlat =
            [|
                for edge, pairs in invalidEdgeArrays do
                    for pair in pairs do
                        pair, edge
            |]

            // look for most common explanations first
        let invalidEdgesGrouped =
            invalidEdgesFlat
                |> Array.unfold (fun edgesFlat ->
                    if edgesFlat.Length = 0 then None
                    else
                            // get the region/result pair with the most common explanation
                        let edgesGrouped =
                            edgesFlat
                                |> Array.groupBy fst
                                |> Array.map (fun (pair, group) ->
                                    pair, Array.map snd group)
                        let maxPair, maxEdges =
                            edgesGrouped
                                |> Array.maxBy (snd >> Array.length)

                            // remove the explained edges from further consideration
                        let edgesFlat =
                            let maxEdgesSet = set maxEdges
                            [|
                                for pair, edges in edgesGrouped do
                                    for edge in edges do
                                        if not (maxEdgesSet.Contains(edge)) then
                                            pair, edge
                            |]

                        Some ((maxPair, maxEdges), edgesFlat))

        {
            Domino = domino
            ValidEdges = validEdges
            InvalidEdgesGrouped = invalidEdgesGrouped
        }

    let print level evidence =

        let indent = System.String(' ', 3 * 2 * level)
        printfn $"{indent}Consider domino {evidence.Domino}:"

        if evidence.ValidEdges.Length > 0 then
            printfn ""
            printfn $"{indent}   It can be placed on the following edges:"
            for edge in evidence.ValidEdges do
                printfn $"{indent}      Edge {edge}"

        if evidence.InvalidEdgesGrouped.Length > 0 then
            printfn ""
            printfn $"{indent}   It cannot be placed on the following edges:"
            for ((region, result), edges) in evidence.InvalidEdgesGrouped do
                printfn $"{indent}      Reason: Region {region.Cells.[0]} becomes invalid ({result})"
                for edge in edges do
                    printfn $"{indent}         Edge {edge}"

        // note: edges not explicitly listed are disallowed by geometry

type PlacementEvidence =
    {
        ParentEvidence : DominoEvidence
        InvalidChildEvidences : (Edge * DominoEvidence)[]
    }

module PlacementEvidence =

    /// Chooses and explains one of the given domino placements.
    let get lookahead domPlacements puzzle =

            // find the easiest placement to explain
        let placementMap = Placement.search 0 puzzle
        let domino, _ =
            domPlacements
                |> Seq.minBy (fun (domino, _ : Placement) ->
                    placementMap[domino].Length)

        let parentEvidence =
            DominoEvidence.get domino puzzle

        let invalidChildEvidences =
            [|
                for placement in placementMap[domino] do
                    let puzzle =
                        Puzzle.place domino placement.Edge puzzle
                    let placementMap = Placement.search 0 puzzle   // deeper searches not yet supported
                    let dominoOpt =
                        puzzle.UnplacedDominoes
                            |> Seq.tryFind (fun domino ->
                                not (placementMap.ContainsKey(domino)))
                    match dominoOpt with
                        | Some domino ->
                            placement.Edge,
                            DominoEvidence.get domino puzzle
                        | None -> ()
            |]

        {
            ParentEvidence = parentEvidence
            InvalidChildEvidences = invalidChildEvidences
        }

    let print evidence =

        let parent = evidence.ParentEvidence
        DominoEvidence.print 0 parent

        if evidence.InvalidChildEvidences.Length > 0 then
            for edge, child in evidence.InvalidChildEvidences do
                printfn ""
                printfn $"   But placing it on edge {edge} leads to a contradiction on the next move:"
                printfn ""
                DominoEvidence.print 1 child

        let edgeStr =
            parent.ValidEdges
                |> Seq.where (fun validEdge ->
                    evidence.InvalidChildEvidences
                        |> Array.forall (fun (invalidEdge, _) ->
                            invalidEdge <> validEdge))
                |> Seq.map (fun edge ->
                    $"edge {edge}")
                |> String.concat " or "
        printfn ""
        printfn $"   Therefore, domino {parent.Domino} must be placed on {edgeStr}"