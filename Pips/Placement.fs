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

        /// Search loop.
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
                            {
                                Domino = domino
                                Edge = edge
                                Children = children
                            }
            |]

            // search using all tilings
        assert(lookahead >= 0)
        let tilingMap =
            Puzzle.getAllTilings puzzle
                |> toTilingMap
        loop lookahead tilingMap puzzle

    let print placement =

        let rec loop depth placement =
            let indent = System.String(' ', 3 * depth)
            printfn $"{indent}{placement.Domino} @ {placement.Edge}"
            for child in placement.Children do
                loop (depth + 1) child

        loop 0 placement
