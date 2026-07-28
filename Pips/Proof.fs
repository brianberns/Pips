namespace Pips

module Proof =

    let private toTilingMap tilings =
        tilings
            |> Seq.collect (fun (tiling : Tiling) ->
                tiling
                    |> Seq.map (fun edge ->
                        let tiling = tiling.Remove(edge)
                        edge, (tiling : Tiling)))
            |> Seq.groupBy fst
            |> Seq.map (fun (edge, group) ->
                edge, Seq.map snd group)
            |> Map

    let solve puzzle =

        let rec search tilingMap puzzle =
            [|
                for domino in puzzle.UnplacedDominoes do
                    for (edge, tilings) in Map.toSeq tilingMap do
                        yield! loop tilings domino edge puzzle
                        if not (Domino.isDouble domino) then
                            let edge = Edge.reverse edge
                            yield! loop tilings domino edge puzzle
            |]

        and loop tilings domino edge puzzle =
            [|
                match Puzzle.tryPlace domino edge puzzle with
                    | Some puzzle ->
                        let tilingMap = toTilingMap tilings
                        yield domino, edge
                        yield! search tilingMap puzzle
                    | None -> ()
            |]

        let tilingMap =
            Puzzle.getAllTilings puzzle
                |> toTilingMap

        search tilingMap puzzle
