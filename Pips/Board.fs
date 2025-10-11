namespace Pips

open System

/// A pair of adjacent cells.
type Edge = Cell * Cell

module Edge =

    /// Determines whether the given edge contains the given cell.
    let contains cell ((cellA, cellB) : Edge) =
        cell = cellA || cell = cellB

    /// Reverses the given edge.
    let reverse ((cellA, cellB) : Edge) : Edge =
        cellB, cellA

/// A 2D lattice with dominoes on it. This is stored in a redundant
/// data structure for speed. We have the location of each domino,
/// and also a way to look up the value at any cell on the board.
[<CustomEquality; NoComparison>]
type Board =
    {
        /// Location of each domino placed on the board.
        Dominoes : List<Domino * Edge>

        /// Value in each cell, if any.
        Cells : PipCount[(*row*), (*column*)]
    }

    /// Pip count of each cell.
    member board.Item(cell) =
        board.Cells[cell.Row, cell.Column]

    /// Domino placement order doesn't matter for equality.
    member this.Equals(other : Board) = 
        (set this.Dominoes) = (set other.Dominoes)

    interface IEquatable<Board> with

        /// Domino placement order doesn't matter for equality.
        member this.Equals(other : Board) = 
            this.Equals(other)

module Board =

    /// Special pip count for an uncovered cell. We use this,
    /// rather than an Option, for speed.
    [<Literal>]
    let emptyCell : PipCount = -1

    /// Creates an empty board of the given size.
    let create numRows numColumns =
        {
            Dominoes = List.empty
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
            Dominoes =
                (domino, edge) :: board.Dominoes
        }
