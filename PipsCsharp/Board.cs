using System.Collections.Immutable;
using System.Data.Common;

namespace PipsCsharp;

/// A 2D grid with dominoes on it. This is stored in a redundant
/// data structure for speed. We have the location of each domino,
/// and also a way to look up the value at any cell on the board.
public class Board
{
    /// Creates an empty board of the given size.
    public Board(int numRows, int numColumns)
    {
        _dominoPlaces = [];
        _cells = new PipCount[numRows, numColumns];
        for (int row = 0; row < numRows; row++)
        {
            for (int col = 0; col < numColumns; col++)
            {
                _cells[row, col] = EmptyCell;
            }
        }
    }

    /// Creates a board in the given state.
    private Board(ImmutableStack<(Domino, Edge)> dominoPlaces, PipCount[,] cells)
    {
        _dominoPlaces = dominoPlaces;
        _cells = cells;
    }

    /// Location of each domino placed on the board.
    private readonly ImmutableStack<(Domino, Edge)> _dominoPlaces;

    /// Value in each cell.
    private readonly PipCount[,] _cells;

    /// Special pip count for an uncovered cell.
    public static PipCount EmptyCell = new PipCount(-1);

    /// Number of rows in the board.
    public int NumRows => _cells.GetLength(0);

    /// Number of columns in the board.
    public int NumColumns => _cells.GetLength(1);

    /// Pip count of each cell.
    public PipCount this[Cell cell]
    {
        get => _cells[cell.Row, cell.Column];
    }

    //    public IEnumerable<(Domino, Edge)> DominoPlaces =>
    //        _dominoPlaces;

    /// Is the given cell empty (i.e. not covered by a domino)?
    public bool IsEmpty(Cell cell) =>
        this[cell] == EmptyCell;

    /// Places the given domino in the given location on the
    /// board. The left side of the domino is placed on the left
    /// cell and the right side of the domino is placed on the
    /// right cell. We use a 2D array for speed.
    public Board Place(Domino domino, Edge edge)
    {
            // place domino
        var dominoPlaces = this._dominoPlaces.Push((domino, edge));

            // copy on write
        var cells = (PipCount[,]) this._cells.Clone();
        cells[edge.CellA.Row, edge.CellA.Column] = domino.Left;
        cells[edge.CellB.Row, edge.CellB.Column] = domino.Right;

        return new Board(dominoPlaces, cells);
    }

    /// Gets all possible cells adjacent to the given cell on
    /// the given board.
    public IEnumerable<Cell> GetAdjacent(Cell cell)
    {
        var nRows = this.NumRows;
        var nColumns = this.NumColumns;
        return cell.Adjacent
            .Where(adj =>
                adj.Row >= 0
                    && adj.Column >= 0
                    && adj.Row < nRows
                    && adj.Column < nColumns);
    }
}
