namespace Pips

/// A 2D array of values. JavaScript doesn't support
/// this directly, so we provide Fable with a jagged
/// array instead.
#if FABLE_COMPILER
type Array2DSafe<'t> = 't[(*row*)][(*column*)]
#else
type Array2DSafe<'t> = 't[(*row*), (*column*)]
#endif

module Array2DSafe =

    /// Creates a 2D array whose elements all have the
    /// given value.
    let create rows cols value : Array2DSafe<_> =
#if FABLE_COMPILER
        Array.init rows (fun _ ->            // a separate array for each row
            Array.create cols value)
#else
        Array2D.create rows cols value
#endif

    /// Copies the given 2D array.
    let inline copy (array : Array2DSafe<'t>) : Array2DSafe<'t> =
#if FABLE_COMPILER
        Array.init array.Length (fun row ->
            Array.copy array[row])
#else
        Array2D.copy array
#endif

    /// Length of dimension 0.
    let inline length0 (array : Array2DSafe<_>) =
#if FABLE_COMPILER
        array.Length
#else
        array.GetLength(0)
#endif

    /// Length of dimension 1.
    let inline length1 (array : Array2DSafe<_>) =
#if FABLE_COMPILER
        array[0].Length
#else
        array.GetLength(1)
#endif

    /// Gets the element with the given indexes.
    let inline getItem row col (array : Array2DSafe<'t>) =
#if FABLE_COMPILER
        array[row][col]
#else
        array[row, col]
#endif

    /// Sets the element with the given indexes.
    let inline setItem row col value (array : Array2DSafe<'t>) =
#if FABLE_COMPILER
        array[row][col] <- value
#else
        array[row, col] <- value
#endif
