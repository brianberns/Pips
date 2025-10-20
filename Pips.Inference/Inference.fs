namespace Pips

module Inference =

    /// Finds all solutions for the given puzzle by inference,
    /// like a human!
    let solve puzzle =

        /// Solves the given puzzle by inference on the given
        /// tiling.
        let rec loop (tiling : Tiling) puzzle =

                // get constraints on edges in this tiling
            let facts = EdgeFact.getAll puzzle tiling

            if facts.Length = 0 then
                if Puzzle.isSolved puzzle then
                    [| puzzle |]
                else Array.empty

            else
                let puzzleOpts =
                    facts
                        |> Seq.tryPick (fun fact ->

                                // get all possible domino placements consistent with this fact
                            let placements =
                                puzzle.UnplacedDominoes
                                    |> Seq.collect (fun domino ->
                                        EdgeFact.apply domino fact
                                            |> Seq.map (fun edge ->
                                                domino, edge))
                                    |> Seq.toArray

                            if placements.Length = 1 then
                                let domino, edge = placements[0]
                                let puzzle =
                                    Puzzle.place domino edge puzzle
                                assert(
                                    tiling.Contains(edge)
                                        || tiling.Contains(Edge.reverse edge))
                                let tiling =
                                    tiling
                                        .Remove(edge)
                                        .Remove(Edge.reverse edge)
                                Some (loop tiling puzzle)
                            else None)

                match puzzleOpts with
                    | Some puzzles -> puzzles
                    | None ->
                        let domino = Seq.head puzzle.UnplacedDominoes
                        let edges =
                            facts
                                |> Seq.collect (fun edgeFact ->
                                    EdgeFact.apply domino edgeFact)
                        [|
                            for edge in edges do
                                let puzzle = Puzzle.place domino edge puzzle
                                assert(
                                    tiling.Contains(edge)
                                        || tiling.Contains(Edge.reverse edge))
                                let tiling =
                                    tiling
                                        .Remove(edge)
                                        .Remove(Edge.reverse edge)
                                yield! loop tiling puzzle
                        |]

        let tiling =
            Puzzle.getAllTilings puzzle
                |> Seq.exactlyOne   // to-do: fix
        loop tiling puzzle
