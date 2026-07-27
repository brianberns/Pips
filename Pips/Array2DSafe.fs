namespace Pips

(*
 * A 2D array of values. JavaScript has no such thing, so under
 * Fable this is a jagged array instead. Both are used through
 * the same functions, so callers don't have to care which one
 * they have.
 *)

#if FABLE_COMPILER

type Array2DSafe<'t> = 't[(*row*)][(*column*)]

module Array2DSafe =

    let create rows cols value : Array2DSafe<_> =
        Array.init rows (fun _ ->            // a separate array for each row
            Array.create cols value)

    let inline copy (array : Array2DSafe<'t>) : Array2DSafe<'t> =
        Array.init array.Length (fun row ->
            Array.copy array[row])

    let inline length0 (array : Array2DSafe<_>) =
        array.Length

    let inline length1 (array : Array2DSafe<_>) =
        array[0].Length

    let inline getItem row col (array : Array2DSafe<'t>) =
        array[row][col]

    let inline setItem row col value (array : Array2DSafe<'t>) =
        array[row][col] <- value

#else

type Array2DSafe<'t> = 't[(*row*), (*column*)]

module Array2DSafe =

    let create rows cols value : Array2DSafe<_> =
        Array2D.create rows cols value

    let inline copy (array : Array2DSafe<_>) : Array2DSafe<_> =
        Array2D.copy array

    let inline length0 (array : Array2DSafe<_>) =
        array.GetLength(0)

    let inline length1 (array : Array2DSafe<_>) =
        array.GetLength(1)

    let inline getItem row col (array : Array2DSafe<'t>) =
        array[row, col]

    let inline setItem row col value (array : Array2DSafe<'t>) =
        array[row, col] <- value

#endif
