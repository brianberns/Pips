namespace Pips

module Inference =

    let solve puzzle =

        let rec loop tiling puzzle =

            let edgeFacts = EdgeFact.getEdgeFacts tiling puzzle

            if edgeFacts.Length = 0 then
                if Puzzle.isSolved puzzle then
                    [| puzzle |]
                else Array.empty
            else
                let puzzleOpts =
                    edgeFacts
                        |> Seq.tryPick (fun edgeFact ->

                            let pairs =
                                puzzle.UnplacedDominoes
                                    |> Seq.collect (fun domino ->
                                        EdgeFact.apply domino edgeFact
                                            |> Seq.map (fun edge ->
                                                domino, edge))
                                    |> Seq.toArray

                            if pairs.Length = 1 then
                                let domino, edge = pairs[0]
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
                            edgeFacts
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
