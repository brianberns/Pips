namespace Pips

module Puzzle =

    let isValidTiled (tilings : seq<Tiling>) puzzle =
        let cellSets =
            puzzle.Regions
                |> Seq.where (fun region ->
                    region.Type = RegionType.Equal)
                |> Seq.map (_.Cells >> set)
                |> Seq.toArray
        let nRequired =
            Set.intersectMany tilings
                |> Seq.choose (fun (cellA, cellB) ->
                    cellSets
                        |> Seq.tryFind (fun cells ->
                            cells.Contains(cellA)
                                && cells.Contains(cellB)))
                |> Seq.length
        let nAvailable =
            puzzle.UnplacedDominoes
                |> Seq.where Domino.isDouble
                |> Seq.length
        nAvailable >= nRequired

