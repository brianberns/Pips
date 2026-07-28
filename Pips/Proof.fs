namespace Pips

type Proof =
    {
        Domino : Domino
        Edge : Edge
        Children : Proof[]
    }

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
                edge, Seq.map snd group |> Seq.toArray)
            |> Map

    let solve lookahead puzzle =

        let rec loop lookahead tilingMap puzzle =
            [|
                for domino in puzzle.UnplacedDominoes do

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

                    for (edge, tilings, puzzle) in tuples do
                        let tilingMap = toTilingMap tilings
                        let children =
                            if lookahead <= 0 then Array.empty
                            else loop (lookahead - 1) tilingMap puzzle
                        if children.Length > 0
                            || puzzle.UnplacedDominoes.IsEmpty
                            || lookahead <= 0 then
                            {
                                Domino = domino
                                Edge = edge
                                Children = children
                            }
            |]

        let tilingMap =
            Puzzle.getAllTilings puzzle
                |> toTilingMap

        assert(lookahead >= 0)
        loop lookahead tilingMap puzzle

    let print proof =

        let rec loop depth proof =
            printfn $"{System.String(' ', 3 * depth)}{proof.Domino} @ {proof.Edge}"
            for child in proof.Children do
                loop (depth + 1) child

        loop 0 proof
