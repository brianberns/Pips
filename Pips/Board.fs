namespace Pips

/// A 2D lattice with dominoes on it. This is stored in a redundant
/// data structure for speed. We have the location of each domino,
/// and also a way to look up the value at any cell on the board.
type Board =
    {
        /// Location of each domino.
        Dominoes : Set<Domino * Cell (*left*) * Cell (*right*)>

        /// Value in each cell.
        Cells : PipCount[(*row*), (*column*)]
    }

    /// Pip count of each cell.
    member board.Item(cell) =
        board.Cells[cell.Row, cell.Column]

module Board =

    /// Special pip count for an uncovered cell. We use this,
    /// rather than an Option, for speed.
    [<Literal>]
    let empty : PipCount = -1

    /// Creates an empty board of the given size.
    let create numRows numColumns =
        {
            Dominoes = Set.empty
            Cells = Array2D.create numRows numColumns empty
        }

    /// Is the given cell empty (i.e. not covered by a domino)?
    let isEmpty (board : Board) cell =
        board[cell] = empty

    /// Places the given domino in the given location on the
    /// board. The left side of the domino is placed on the left
    /// cell and the right side of the domino is placed on the
    /// right cell. We use a 2D array for speed.
    let place domino cellLeft cellRight board =
        assert(Cell.areAdjacent cellLeft cellRight)
        assert(isEmpty board cellLeft)
        assert(isEmpty board cellRight)

            // copy on write
        let cells = Array2D.copy board.Cells
        cells[cellLeft.Row, cellLeft.Column] <- domino.Left
        cells[cellRight.Row, cellRight.Column] <- domino.Right

        {
            Cells = cells
            Dominoes =
                board.Dominoes
                    |> Set.add (domino, cellLeft, cellRight)
        }
