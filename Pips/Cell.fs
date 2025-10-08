namespace Pips

[<Struct>]
type Cell =
    {
        Row : int
        Column : int
    }

module Cell =

    /// Gets all possible cells adjacent to the given cell.
    /// Some of these cells might not actually exist, though.
    let getAdjacent cell =
        [|
            { cell with Row = cell.Row - 1 }
            { cell with Row = cell.Row + 1 }
            { cell with Column = cell.Column - 1 }
            { cell with Column = cell.Column + 1 }
        |]

    let adjacent cellA cellB =
        (cellA.Row = cellB.Row
            && abs (cellA.Column - cellB.Column) = 1)
            || (cellA.Column = cellB.Column
                && abs (cellA.Row - cellB.Row) = 1)
