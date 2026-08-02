namespace Pips

module Puzzle =

    /// Is the given puzzle in a valid state for the
    /// given tilings?
    let isValidTiled (tilings : seq<Tiling>) puzzle =

            // gather cells that are in `Equal` regions
        let cellSets =
            puzzle.Regions
                |> Seq.where (fun region ->
                    region.Type = RegionType.Equal)
                |> Seq.map (_.Cells >> set)
                |> Seq.toArray

            // look for required edges that are wholy contained in an `Equal` region
        let nRequired =
            Set.intersectMany tilings
                |> Seq.choose (fun (cellA, cellB) ->
                    cellSets
                        |> Seq.tryFind (fun cells ->
                            cells.Contains(cellA)
                                && cells.Contains(cellB)))
                |> Seq.length

            // such edges require doubles
        let nAvailable =
            puzzle.UnplacedDominoes
                |> Seq.where Domino.isDouble
                |> Seq.length

        nAvailable >= nRequired
