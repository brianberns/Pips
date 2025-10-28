namespace Pips

type Array2DSafe<'t> = 't[(*row*)][(*column*)]

module Array2DSafe =

    let create rows cols value : Array2DSafe<_> =
        Array.create rows (
            Array.create cols value)

    let copy (array : Array2DSafe<'t>) : Array2DSafe<'t> =
        Array.init array.Length (fun r ->
            Array.copy array[r])

    let inline length0 (array : Array2DSafe<_>) =
        array.Length

    let inline length1 (array : Array2DSafe<_>) =
        array[0].Length

    let getItem row col (array : Array2DSafe<'t>) =
        array[row][col]

    let setItem row col value (array : Array2DSafe<'t>) =
        array[row][col] <- value
