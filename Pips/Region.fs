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

/// Unplaced pip counts.
type UnplacedPipCounts =
    {
        /// Unsorted.
        Unsorted : PipCount[]

        /// Sorted in ascending order.
        Ascending : PipCount[]

        /// Sorted in descending order.
        Descending : PipCount[]
    }

module UnplacedPipCounts =

    /// Creates unplaced pip counts.
    let create unsorted =
        let sorted = Array.sort unsorted
        {
            Unsorted = unsorted
            Ascending = sorted
            Descending = Array.rev sorted
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
    let private validateSumLess board unplacedPipCounts target region =
        assert(region.Type.IsSumLess)

        let pipCounts = getPipCounts board region
        let sum = Array.sum pipCounts

            // target already exceeded? (assume no negative pip counts)
        if sum > target then false

            // are there enough small values available?
        else
            let nNeeded =
                region.Cells.Length - pipCounts.Length
            let smallest =
                unplacedPipCounts.Ascending[0 .. nNeeded-1]
                    |> Array.sum
            sum + smallest < target

    /// Validates a SumGreater region.
    let private validateSumGreater board unplacedPipCounts target region =
        assert(region.Type.IsSumGreater)

        let pipCounts = getPipCounts board region
        let sum = Array.sum pipCounts

            // are there enough large values available?
        let nNeeded =
            region.Cells.Length - pipCounts.Length
        let largest =
            unplacedPipCounts.Descending[0 .. nNeeded-1]
                |> Array.sum
        sum + largest > target

    /// Determines if k elements can sum to the given target.
    let private canSum source target k =

            // dp[j][c] will be true if sum 'j' is possible with 'c' items
        let dp = Array2D.create (target + 1) (k + 1) false
    
            // base case: a sum of 0 with 0 items is always possible
        dp[0, 0] <- true

            // if we can make sum (j - num) with (c - 1) items, we can
            // make sum j with c items
        for elem in source do
            for j = target downto elem do
                for c = k downto 1 do
                    if dp[j - elem, c - 1] then
                        dp[j, c] <- true
                
        dp[target, k]

    /// Validates a SumExact region.
    let private validateSumExact board unplacedPipCounts target region =
        assert(region.Type.IsSumExact)

        let pipCounts = getPipCounts board region
        let sum = Array.sum pipCounts

            // target already exceeded? (assume no negative pip counts)
        if sum > target then false
        else
            let nNeeded =
                region.Cells.Length - pipCounts.Length

                // must hit target exactly when all cells covered
            if nNeeded = 0 then
                sum = target

            else
                    // are there enough small values available?
                let valid =
                    let smallest =
                        unplacedPipCounts.Ascending[0 .. nNeeded-1]
                            |> Array.sum
                    sum + smallest <= target

                    // are there enough large values available?
                let valid =
                    if valid then
                        let largest =
                            unplacedPipCounts.Descending[0 .. nNeeded-1]
                                |> Array.sum
                        sum + largest >= target
                    else false

                if valid then
                    canSum
                        unplacedPipCounts.Unsorted
                        (target - sum)
                        nNeeded
                else false

    /// Validates the given region on the given board with the
    /// given unplaced dominoes.
    let isValid board unplacedPipCounts region =
        match region.Type with
            | RegionType.Any -> true
            | RegionType.Equal ->
                validateEqual board unplacedPipCounts region
            | RegionType.Unequal ->
                validateUnequal board unplacedPipCounts region
            | RegionType.SumLess target ->
                validateSumLess board unplacedPipCounts target region
            | RegionType.SumGreater target ->
                validateSumGreater board unplacedPipCounts target region
            | RegionType.SumExact target ->
                validateSumExact board unplacedPipCounts target region

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
                | RegionType.SumLess target ->
                    Array.sum pipCounts < target
                | RegionType.SumGreater target ->
                    Array.sum pipCounts > target
                | RegionType.SumExact target ->
                    Array.sum pipCounts = target
        else false
