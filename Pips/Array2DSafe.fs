namespace Pips

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
