namespace Pips

module Proof =

    let rec solve (puzzle : Puzzle) =

        let edges = 
            Puzzle.getAllTilings puzzle
                |> Seq.concat
                |> Seq.distinct

        seq {
            for domino in puzzle.UnplacedDominoes do
                for edge in edges do
                    match Puzzle.tryPlace domino edge puzzle with
                        | Some puzzle ->
                            yield domino, edge
                            yield! solve puzzle
                        | None -> ()
        }
