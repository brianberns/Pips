namespace Pips

/// A region type defines a constraint on the cells in a
/// region.
[<RequireQualifiedAccess>]
type RegionType =

    /// Cells in the region can have any value.
    | Any

    /// All cells in the region must have the same value.
    | Equal

    /// All cells in the region must have a distinct value.
    | Unequal

    /// Sum of cell values in the region must be less than
    /// a certain amount.
    | SumLess of int

    /// Sum of cell values in the region must be greater than
    /// a certain amount.
    | SumGreater of int

    /// Sum of cell values in the region must be equal to
    /// a certain amount.
    | SumExact of int

/// A region of cells on a board.
type Region =
    {
        /// Cells in the region.
        Cells : Cell[]

        /// Constraint on the cells in the region.
        Type : RegionType
    }

module Region =

    /// Gets the pip counts covering cells in the given region
    /// on the given board.
    let private getPipCounts (board : Board) region =
        region.Cells
            |> Array.map board.Item
            |> Array.where ((<>) Board.emptyCell)

    /// Determines whether the given array has fewer than two
    /// distinct elements.
    let private hasLessThanTwoDistinct (array : _[]) =
        if array.Length < 2 then true
        else
            let elem = array[0]
            Array.forall ((=) elem) array[1..]   // fail early if a second value is found

    /// Validates an Equal region.
    let private validateEqual board region =
        assert(region.Type.IsEqual)
        let pipCounts = getPipCounts board region
        hasLessThanTwoDistinct pipCounts

    /// Validates an Unequal region.
    let private validateUnequal board region =
        assert(region.Type.IsUnequal)
        let pipCounts = getPipCounts board region
        (Array.distinct pipCounts).Length = pipCounts.Length

    /// Validates a SumLess region.
    let private validateSumLess n board region =
        assert(region.Type.IsSumLess)
        let pipCounts = getPipCounts board region
        assert(PipCount.minValue = 0)
        Array.sum pipCounts < n

    /// Validates a SumGreater region.
    let private validateSumGreater n board region =
        assert(region.Type.IsSumGreater)
        let pipCounts = getPipCounts board region

            // are there still enough uncovered cells to exceed the target?
        let nEmpty = region.Cells.Length - pipCounts.Length
        (Array.sum pipCounts) + (PipCount.maxValue * nEmpty) > n

    /// Validates a Sum region.
    let private validateSum n board region =
        assert(region.Type.IsSumExact)
        let pipCounts = getPipCounts board region
        let sum = Array.sum pipCounts

            // all cells covered?
        if pipCounts.Length = region.Cells.Length then
            sum = n
        else
            assert(PipCount.minValue = 0)

                // are there still enough uncovered cells to reach the target?
            if sum <= n then
                let nEmpty = region.Cells.Length - pipCounts.Length
                sum + (PipCount.maxValue * nEmpty) >= n
            else false

    /// Validates the given region on the given board. The region
    /// may still have uncovered cells.
    let isValid board region =
        match region.Type with
            | RegionType.Any -> true
            | RegionType.Equal ->
                validateEqual board region
            | RegionType.Unequal ->
                validateUnequal board region
            | RegionType.SumLess n ->
                validateSumLess n board region
            | RegionType.SumGreater n ->
                validateSumGreater n board region
            | RegionType.SumExact n ->
                validateSum n board region

    /// Determines whether the given region on the given board has
    /// been solved (with no uncovered cells).
    let isSolved board region =
        let pipCounts = getPipCounts board region
        if pipCounts.Length = region.Cells.Length then
            match region.Type with
                | RegionType.Any -> true
                | RegionType.Equal ->
                    (Array.distinct pipCounts).Length = 1
                | RegionType.Unequal ->
                    (Array.distinct pipCounts).Length = pipCounts.Length
                | RegionType.SumLess n ->
                    Array.sum pipCounts < n
                | RegionType.SumGreater n ->
                    Array.sum pipCounts > n
                | RegionType.SumExact n ->
                    Array.sum pipCounts = n
        else false
