namespace PipsCsharp;

/// A cell in a grid.
public record Cell(int Row, int Column)
{
    /// Display string.
    public override string ToString() =>
        $"({Row}, {Column})";

    /// Gets all possible cells adjacent to this cell. Some
    /// of these cells might not actually exist, though.
    public IEnumerable<Cell> Adjacent
    {
        get {
            yield return this with { Row = this.Row - 1 };
            yield return this with { Row = this.Row + 1 };
            yield return this with { Column = this.Column - 1 };
            yield return this with { Column = this.Column + 1 };
        }
    }

    /// Determines whether the given cells are adjacent.
    public static bool AreAdjacent(Cell cellA, Cell cellB) =>
        (cellA.Row == cellB.Row
            && Math.Abs(cellA.Column - cellB.Column) == 1)
            || (cellA.Column == cellB.Column
                && Math.Abs(cellA.Row - cellB.Row) == 1);
}

/// A pair of adjacent cells.
public record Edge(Cell CellA, Cell CellB)
{
    /// Does this edge contain the given cell?
    public bool Contains(Cell cell) =>
        cell == CellA || cell == CellB;

    /// Reverses this edge.
    public Edge Reverse() =>
        new(this.CellB, this.CellA);
}
