namespace Pips

/// A 2D grid with dominoes on it. This is stored in a redundant
/// data structure for speed. We have the location of each domino,
/// and also a way to look up the value at any cell on the board.
[<NoEquality; NoComparison>]
type Board =
    {
        /// Location of each domino placed on the board.
        DominoPlaces : List<Domino * Edge>

        /// Value in each cell.
        Cells : Array2DSafe<PipCount>
    }

    /// Number of rows in the board.
    member board.NumRows =
        Array2DSafe.length0 board.Cells

    /// Number of columns in the board.
    member board.NumColumns =
        Array2DSafe.length1 board.Cells

    /// Pip count of the given cell.
    member board.Item(cell) =
        Array2DSafe.getItem
            cell.Row cell.Column board.Cells

module Board =

    /// Special pip count for an uncovered cell. We use this,
    /// rather than an Option, for speed.
    [<Literal>]
    let emptyCell : PipCount = -1

    /// Creates an empty board of the given size.
    let create numRows numColumns =
        {
            DominoPlaces = List.empty
            Cells = Array2DSafe.create numRows numColumns emptyCell
        }

    /// Is the given cell empty (i.e. not covered by a domino)?
    let isEmpty cell (board : Board) =
        board[cell] = emptyCell

    /// Places the given domino on the given empty edge on the 
    /// board. The left side of the domino is placed on the left
    /// cell and the right side of the domino is placed on the
    /// right cell.
    let place domino ((cellLeft, cellRight) as edge : Edge) board =
        assert(Cell.areAdjacent cellLeft cellRight)
        assert(isEmpty cellLeft board)
        assert(isEmpty cellRight board)

            // copy on write
        let cells = Array2DSafe.copy board.Cells
        Array2DSafe.setItem
            cellLeft.Row cellLeft.Column domino.Left cells
        Array2DSafe.setItem
            cellRight.Row cellRight.Column domino.Right cells
        {
            Cells = cells
            DominoPlaces =
                (domino, edge) :: board.DominoPlaces
        }

    /// Gets all cells adjacent to the given cell on the given
    /// board.
    let getAdjacent cell (board : Board) =
        let nRows = board.NumRows
        let nCols = board.NumColumns
        Cell.getAdjacent cell
            |> Seq.where (fun adj ->
                adj.Row >= 0
                    && adj.Column >= 0
                    && adj.Row < nRows
                    && adj.Column < nCols)
