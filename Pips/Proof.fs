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

    type Proof =
        {
            Domino : Domino
            Edge : Edge
            Children : seq<Proof>
        }

    let solve puzzle =

        let rec loop tilingMap puzzle =
            seq {
                for domino in puzzle.UnplacedDominoes do
                    let pairs =
                        seq {
                            for (edge, tilings) in Map.toSeq tilingMap do

                                let canPlace edge =
                                    Puzzle.tryPlace domino edge puzzle
                                        |> Option.isSome

                                if canPlace edge then edge, tilings

                                if not (Domino.isDouble domino) then
                                    let edge = Edge.reverse edge
                                    if canPlace edge then edge, tilings
                        }

                    match Seq.tryExactlyOne pairs with
                        | Some (edge, tilings) ->
                            let puzzle = Puzzle.place domino edge puzzle
                            let tilingMap = toTilingMap tilings
                            {
                                Domino = domino
                                Edge = edge
                                Children = loop tilingMap puzzle
                            }
                        | None -> ()
            }

        let tilingMap =
            Puzzle.getAllTilings puzzle
                |> toTilingMap

        loop tilingMap puzzle
