namespace Pips

type Cell =
    {
        Row : int
        Column : int
    }

module Cell =

    let adjacent cellA cellB =
        (cellA.Row = cellB.Row
            && abs (cellA.Column - cellB.Column) = 1)
            || (cellA.Column = cellB.Column
                && abs (cellA.Row - cellB.Row) = 1)
