namespace Pips

/// A cell in a grid.
type Cell =
    {
        /// Row coordinate (0-based).
        Row : int

        /// Column coordinate (0-based).
        Column : int
    }

    /// Display string.
    override cell.ToString() =
        $"({cell.Row}, {cell.Column})"

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

/// A pair of adjacent cells.
type Edge = Cell * Cell

module Edge =

    /// Does the given edge contain the given cell?
    let contains cell ((cellA, cellB) : Edge) =
        cell = cellA || cell = cellB

    /// Reverses the given edge.
    let reverse ((cellA, cellB) : Edge) : Edge =
        cellB, cellA
