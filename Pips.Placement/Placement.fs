namespace Pips

/// Placement of a domino on an edge.
type Placement =
    {
        /// Edge on which the domino is being placed.
        Edge : Edge

        /// Subsequent placements of remaining dominoes.
        ChildMap : PlacementMap
    }

and PlacementMap = Map<Domino, Placement[]>

module Placement =

    /// Creates a placement.
    let create edge childMap =
        {
            Edge = edge
            ChildMap = childMap
        }

    /// Maps each edge in the given tilings to the
    /// tilings in which it appears.
    let private toTilingMap tilings =
        tilings
            |> Seq.collect (fun (tiling : Tiling) ->
                tiling
                    |> Seq.map (fun edge ->
                        let tiling = tiling.Remove(edge)   // consume edge
                        edge, (tiling : Tiling)))
            |> Seq.groupBy fst
            |> Seq.map (fun (edge, group) ->
                edge, Seq.map snd group |> Seq.toArray)
            |> Map

    let private tryPlace domino edge tilings puzzle =
        option {
            let! puzzle =
                Puzzle.tryPlace domino edge puzzle
            if Puzzle.isValidTiled tilings puzzle then
                return puzzle
        }

    /// Searches the given puzzle for valid placements.
    let search lookahead puzzle =

        let rec loop lookahead tilingMap puzzle : PlacementMap =

                // prepare possible edges
            let withoutReverse =
                Map.toArray tilingMap
            let withReverse =
                [|
                    for (edge, tilings) in Map.toSeq tilingMap do
                        edge, tilings
                        Edge.reverse edge, tilings
                |]

            Map [|
                    // attempt to place each domino
                for domino in puzzle.UnplacedDominoes do

                        // don't reverse edges for doubles
                    let pairs =
                        if Domino.isDouble domino then withoutReverse
                        else withReverse

                        // attempt to place domino on available edges
                    let placements =
                        pairs
                            |> Array.Parallel.choose (fun (edge, tilings) ->
                                option {
                                    let! puzzle =
                                        tryPlace domino edge tilings puzzle
                                    if puzzle.UnplacedDominoes.IsEmpty   // puzzle is solved
                                        || lookahead <= 0 then           // lookahead horizon reached
                                        return create edge Map.empty
                                    else
                                        let childMap =
                                            let tilingMap = toTilingMap tilings
                                            loop (lookahead - 1) tilingMap puzzle
                                        assert(childMap.Count <= puzzle.UnplacedDominoes.Count)
                                        if childMap.Count = puzzle.UnplacedDominoes.Count then
                                            return create edge childMap
                                })
                    if placements.Length > 0 then
                        domino, placements
            |]

            // start search with all tilings
        assert(lookahead >= 0)
        let tilingMap =
            Puzzle.getAllTilings puzzle
                |> toTilingMap
        loop lookahead tilingMap puzzle

    let print placementMap =

        let rec loop depth (placementMap : PlacementMap) =
            let indent = System.String(' ', 3 * depth)
            for domino, placements in Map.toSeq placementMap do
                if placements.Length = 0 then
                    printfn $"{indent}{domino} at None"
                else
                    for placement in placements do
                        printfn $"{indent}{domino} at {placement.Edge}"
                        loop (depth + 1) placement.ChildMap

        loop 0 placementMap

type ForcedPlacementReason =
    | Lookahead of int

type ForcedPlacement =
    {
        Domino : Domino
        Edges : Edge[]
        Reason : ForcedPlacementReason
    }

module ForcedPlacement =

    let create domino edges reason =
        {
            Domino = domino
            Edges = edges
            Reason = reason
        }

    let private tryForce lookahead domino placements =
        let cellSets =
            placements
                |> Array.map (fun placement ->
                    let (cellA, cellB) = placement.Edge
                    set [ cellA; cellB ])
                |> set
        if cellSets.Count = 1 then
            let edges =
                placements
                    |> Seq.map _.Edge
                    |> Seq.toArray
            Some (create domino edges (Lookahead lookahead))
        else None

    /// Searches the given puzzle for forced placements.
    let search maxLookahead puzzle =
        [ 0 .. maxLookahead ]
            |> Seq.tryPick (fun lookahead ->
                Placement.search lookahead puzzle
                    |> Map.toSeq
                    |> Seq.tryPick (fun (domino, placements) ->
                        tryForce lookahead domino placements))
