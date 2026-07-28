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

                    let pairs =
                        [|
                            for (edge, tilings) in Map.toSeq tilingMap do
                                let canPlace edge =
                                    Puzzle.tryPlace domino edge puzzle
                                        |> Option.isSome
                                if canPlace edge then edge, tilings
                                if not (Domino.isDouble domino) then
                                    let edge = Edge.reverse edge
                                    if canPlace edge then edge, tilings
                        |]

                    if pairs.Length = 1 then

                        let edge, tilings = Array.exactlyOne pairs
                        yield domino, edge

                        let puzzle = Puzzle.place domino edge puzzle
                        let tilingMap = toTilingMap tilings
                        yield! search tilingMap puzzle
            |]

        let tilingMap =
            Puzzle.getAllTilings puzzle
                |> toTilingMap

        search tilingMap puzzle
