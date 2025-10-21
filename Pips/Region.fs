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

    module private Seq =

        /// Determines whether the given sequence contains at
        /// least the given number of elements.
        let isLengthAtLeast n source =
            source
                |> Seq.truncate n
                |> Seq.length = n

    type UnplacedPipCounts =
        {
            Unsorted : PipCount[]
            SortedAscending : Lazy<PipCount[]>
            SortedDescending : Lazy<PipCount[]>
        }

    module UnplacedPipCounts =

        let create pipCounts =
            {
                Unsorted = pipCounts
                SortedAscending =
                    lazy (Array.sort pipCounts)
                SortedDescending =
                    lazy (Array.sortDescending pipCounts)
            }

    /// Validates an Equal region.
    let private validateEqual board unplacedPipCounts region =
        assert(region.Type.IsEqual)

        let pipCounts = getPipCounts board region

            // are all values equal so far?
        let equal =
            if pipCounts.Length < 2 then true
            else
                let value = pipCounts[0]
                Array.forall ((=) value) pipCounts[1..]   // fail early if a second value is found

            // are there enough matching values available?
        if equal && pipCounts.Length > 0 then
            let value = pipCounts[0]
            let nNeeded =
                region.Cells.Length - pipCounts.Length
            unplacedPipCounts.Unsorted
                |> Seq.where ((=) value)
                |> Seq.isLengthAtLeast nNeeded

        else equal

    /// Validates an Unequal region.
    let private validateUnequal board unplacedPipCounts region =
        assert(region.Type.IsUnequal)

        let pipCounts = getPipCounts board region

            // are all values distinct so far?
        let distinctValues = Array.distinct pipCounts
        if distinctValues.Length = pipCounts.Length then

                // are there enough distinct values available?
            let distinctValues = set distinctValues
            let nNeeded =
                region.Cells.Length - pipCounts.Length
            unplacedPipCounts.Unsorted
                |> Seq.where (
                    distinctValues.Contains >> not)
                |> Seq.distinct
                |> Seq.isLengthAtLeast nNeeded

        else false

    /// Validates a SumLess region.
    let private validateSumLess board unplacedPipCounts n region =
        assert(region.Type.IsSumLess)

        let pipCounts = getPipCounts board region

            // are there enough small values available?
        let nNeeded =
            region.Cells.Length - pipCounts.Length
        let smallest =
            unplacedPipCounts.SortedAscending.Value
                |> Seq.take nNeeded
                |> Seq.sum
        Array.sum pipCounts + smallest < n

    /// Validates a SumGreater region.
    let private validateSumGreater board unplacedPipCounts n region =
        assert(region.Type.IsSumGreater)

        let pipCounts = getPipCounts board region

            // are there enough large values available?
        let nNeeded =
            region.Cells.Length - pipCounts.Length
        let largest =
            unplacedPipCounts.SortedDescending.Value
                |> Seq.take nNeeded
                |> Seq.sum
        Array.sum pipCounts + largest > n

    /// Validates a Sum region.
    let private validateSum board unplacedPipCounts n region =
        assert(region.Type.IsSumExact)

        let pipCounts = getPipCounts board region
        let sum = Array.sum pipCounts
        let nNeeded =
            region.Cells.Length - pipCounts.Length

            // all cells covered?
        if nNeeded = 0 then
            sum = n

        else
                // are there enough small values available?
            let valid =
                let smallest =
                    unplacedPipCounts.SortedAscending.Value
                        |> Seq.take nNeeded
                        |> Seq.sum
                sum + smallest <= n

                // are there enough large values available?
            if valid then
                let nNeeded =
                    region.Cells.Length - pipCounts.Length
                let largest =
                    unplacedPipCounts.SortedDescending.Value
                        |> Seq.take nNeeded
                        |> Seq.sum
                sum + largest >= n
            else false

    /// Validates the given region on the given board with the
    /// given unplaced dominoes.
    let isValid board unplacedPipCounts region =
        match region.Type with
            | RegionType.Any -> true
            | RegionType.Equal ->
                validateEqual
                    board unplacedPipCounts region
            | RegionType.Unequal ->
                validateUnequal
                    board unplacedPipCounts region
            | RegionType.SumLess n ->
                validateSumLess
                    board unplacedPipCounts n region
            | RegionType.SumGreater n ->
                validateSumGreater
                    board unplacedPipCounts n region
            | RegionType.SumExact n ->
                validateSum
                    board unplacedPipCounts n region

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
