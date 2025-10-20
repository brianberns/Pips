namespace Pips

module Region =

    /// Tightens the defintion of the given region, if possible.
    let tighten board (region : Region) =

            // group covered and uncovered cells within the region
        let uncovered, covered =
            Array.partition
                (flip Board.isEmpty board)
                region.Cells

            // get the values in the covered cells
        let values = Array.map board.Item covered

        /// Creates a new region for the uncovered cells with a
        /// (potentially) reduced target.
        let decreaseTarget target mkRegionType =

                // compute reduced target
            let target = target - Array.sum values
            assert(target >= PipCount.minValue * uncovered.Length)

                // create new region from remaining cells
            Array.singleton {
                Cells = uncovered
                Type = mkRegionType target
            }

        match region.Type with

                // if at least one cell is covered, all cells in the
                // region must have the value of the covered cell(s)
            | RegionType.Equal when covered.Length > 0 ->
                let target =
                    Seq.distinct values |> Seq.exactlyOne

                    // break the region into individual cells constained
                    // to the known value
                uncovered
                    |> Array.map (fun cell ->
                        {
                            Cells = [| cell |]
                            Type = RegionType.SumExact target
                        })

                // attempt to decrease region target
            | RegionType.SumLess target ->
                decreaseTarget target RegionType.SumLess

                // attempt to decrease region target
            | RegionType.SumExact target ->
                decreaseTarget target RegionType.SumExact

                // no-op
            | _ -> [| region |]
