namespace Pips

/// A cell on a 2D lattice.
type Cell =
    {
        /// 0-based row.
        Row : int

        /// 0-based column.
        Column : int
    }

module Cell =

    /// Creates a cell.
    let create row col =
        { Row = row; Column = col }

    /// Gets all possible cells adjacent to the given cell.
    /// Some of these cells might not actually exist, though.
    let getAdjacent cell =
        [|
            { cell with Row = cell.Row - 1 }
            { cell with Row = cell.Row + 1 }
            { cell with Column = cell.Column - 1 }
            { cell with Column = cell.Column + 1 }
        |]

    /// Determines whether the given cells are adjacent.
    let areAdjacent cellA cellB =
        (cellA.Row = cellB.Row
            && abs (cellA.Column - cellB.Column) = 1)
            || (cellA.Column = cellB.Column
                && abs (cellA.Row - cellB.Row) = 1)
