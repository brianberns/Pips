namespace Pips

module Puzzle =

    let private validateTiled puzzle =

            // gather cells that are in `Equal` regions
        let cellSetRegions =
            puzzle.Regions
                |> Seq.where (fun region ->
                    region.Type = RegionType.Equal)
                |> Seq.map (fun region ->
                    set region.Cells, region)
                |> Seq.toArray

            // look for required edges that are wholy contained in an `Equal` region
        let required =
            puzzle
                |> Puzzle.getAllTilings
                |> Set.intersectMany
                |> Seq.choose (fun (cellA, cellB) ->
                    cellSetRegions
                        |> Seq.tryFind (fun (cells, _) ->
                            cells.Contains(cellA)
                                && cells.Contains(cellB)))
                |> Seq.toArray

            // such edges require doubles
        let nAvailable =
            puzzle.UnplacedDominoes
                |> Seq.where Domino.isDouble
                |> Seq.length

        if nAvailable >= required.Length then Array.empty
        else
            required
                |> Array.map (fun (_, region) ->
                    region, NotEnough)

    /// Places the given domino in the given location in
    /// the given puzzle, if possible.
    let tryPlaceTiledValid domino edge puzzle =
        assert((validateTiled puzzle).Length = 0)

            // try basic placement
        let result = Puzzle.tryPlaceValid domino edge puzzle

            // apply tiled validation rules?
        match result with
            | Ok puzzle ->
                let pairs = validateTiled puzzle   // validate the updated puzzle
                if pairs.Length = 0 then Ok puzzle
                else Error pairs
            | Error pairs -> Error pairs

    /// Places the given domino in the given location in
    /// the given puzzle, if possible.
    let tryPlaceTiled domino edge puzzle =
        match tryPlaceTiledValid domino edge puzzle with
            | Ok puzzle -> Some puzzle
            | Error _ -> None
