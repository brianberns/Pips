namespace Pips

type Explanation =
    {
        Domino : Domino
        ValidEdges : Edge[]
        InvalidEdgesGrouped : ((Region * ValidationResult) * Edge[])[]
    }

module Explain =

    /// Explains where the given domino can and can't be placed
    /// in the given puzzle.
    let explainDomino domino puzzle =

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

    let print explanation =

        printfn $"Domino {explanation.Domino}"
        for edge in explanation.ValidEdges do
            printfn $"   Edge {edge} is valid"
        for ((region, result), edges) in explanation.InvalidEdgesGrouped do
            printfn $"   Region {region.Cells.[0]} invalid because {result}"
            for edge in edges do
                printfn $"      Edge {edge}"
    