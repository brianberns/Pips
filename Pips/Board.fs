namespace Pips

open System

/// A pair of adjacent cells.
type Edge = Cell * Cell

module Edge =

    /// Does the given edge contain the given cell?
    let contains cell ((cellA, cellB) : Edge) =
        cell = cellA || cell = cellB

    /// Reverses the given edge.
    let reverse ((cellA, cellB) : Edge) : Edge =
        cellB, cellA

/// A 2D lattice with dominoes on it. This is stored in a redundant
/// data structure for speed. We have the location of each domino,
/// and also a way to look up the value at any cell on the board.
[<CustomEquality; CustomComparison>]
type Board =
    {
        /// Location of each domino placed on the board.
        /// (Semantically, this is a set, but is stored as a list
        /// for speed.)
        DominoPlaces : List<Domino * Edge>

        /// Value in each cell, if any.
        Cells : PipCount[(*row*), (*column*)]
    }

    /// Pip count of each cell.
    member board.Item(cell) =
        board.Cells[cell.Row, cell.Column]

    /// Equality key.
    member private board.DominoPlacesSet =
        lazy set board.DominoPlaces

    /// Equality override.
    override board.Equals(other) =
        board.Equals(other :?> Board)

    /// Hash code override.
    override board.GetHashCode() =
        board.DominoPlacesSet.Value.GetHashCode()

    /// Domino placement order doesn't matter for equality.
    member board.Equals(other : Board) = 
        board.DominoPlacesSet.Value = other.DominoPlacesSet.Value

    interface IEquatable<Board> with

        /// Domino placement order doesn't matter for equality.
        member board.Equals(other : Board) = 
            board.Equals(other)

    /// Domino placement order doesn't matter for comparison.
    member board.CompareTo(other : Board) =
        compare
            board.DominoPlacesSet.Value
            other.DominoPlacesSet.Value

    interface IComparable with

        /// Domino placement order doesn't matter for comparison.
        member board.CompareTo(other) =
            board.CompareTo(other :?> Board)

    interface IComparable<Board> with

        /// Domino placement order doesn't matter for comparison.
        member board.CompareTo(other) =
            board.CompareTo(other)

module Board =

    /// Special pip count for an uncovered cell. We use this,
    /// rather than an Option, for speed.
    [<Literal>]
    let emptyCell : PipCount = -1

    /// Creates an empty board of the given size.
    let create numRows numColumns =
        {
            DominoPlaces = List.empty
            Cells = Array2D.create numRows numColumns emptyCell
        }

    /// Is the given cell empty (i.e. not covered by a domino)?
    let isEmpty (board : Board) cell =
        board[cell] = emptyCell

    /// Places the given domino in the given location on the
    /// board. The left side of the domino is placed on the left
    /// cell and the right side of the domino is placed on the
    /// right cell. We use a 2D array for speed.
    let place domino ((cellLeft, cellRight) as edge : Edge) board =
        assert(Cell.areAdjacent cellLeft cellRight)
        assert(isEmpty board cellLeft)
        assert(isEmpty board cellRight)

            // copy on write
        let cells = Array2D.copy board.Cells
        cells[cellLeft.Row, cellLeft.Column] <- domino.Left
        cells[cellRight.Row, cellRight.Column] <- domino.Right

        {
            Cells = cells
            DominoPlaces =
                (domino, edge) :: board.DominoPlaces
        }

    /// Gets all possible cells adjacent to the given cell on
    /// the given board.
    let getAdjacent cell board =
        Cell.getAdjacent cell
            |> Seq.where (fun adj ->
                adj.Row >= 0
                    && adj.Column >= 0
                    && adj.Row < board.Cells.GetLength(0)
                    && adj.Column < board.Cells.GetLength(1))
