namespace Pips

/// Placement of a domino on an edge.
type Placement =
    {
        /// Domino being placed.
        Domino : Domino

        /// Edge on which the domino is being placed.
        Edge : Edge

        /// Subsequent placements.
        Children : Placement[]
    }

module Placement =

    /// Creates a placement.
    let create domino edge children =
        {
            Domino = domino
            Edge = edge
            Children = children
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

    /// Searches the given puzzle for valid placements.
    let search lookahead puzzle =

        let rec loop lookahead tilingMap puzzle =
            [|
                    // attempt to place each domino
                for domino in puzzle.UnplacedDominoes do

                        // attempt to place domino on available edges
                    let tuples =
                        let reverse = not (Domino.isDouble domino)
                        Array.choose (fun (edge, tilings) ->
                            puzzle
                                |> Puzzle.tryPlace domino edge
                                |> Option.map (fun puzzle ->
                                    edge, tilings, puzzle))
                            [|
                                for (edge, tilings) in Map.toSeq tilingMap do
                                    edge, tilings
                                    if reverse then
                                        Edge.reverse edge, tilings
                            |]

                        // recurse and construct placements
                    for (edge, tilings, puzzle) in tuples do

                            // loop for child placements?
                        let children =
                            if lookahead <= 0 then Array.empty
                            else
                                let tilingMap = toTilingMap tilings
                                loop (lookahead - 1) tilingMap puzzle

                            // valid placement found, or end of search
                        if children.Length > 0
                            || puzzle.UnplacedDominoes.IsEmpty
                            || lookahead <= 0 then
                            create domino edge children
            |]

            // start search with all tilings
        assert(lookahead >= 0)
        let tilingMap =
            Puzzle.getAllTilings puzzle
                |> toTilingMap
        loop lookahead tilingMap puzzle

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
                    |> Array.groupBy _.Domino
                    |> Array.tryPick (fun (domino, placements) ->
                        tryForce lookahead domino placements))
