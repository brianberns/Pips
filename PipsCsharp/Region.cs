namespace PipsCsharp;

public abstract class RegionType
{
    public abstract bool IsValid(Region region, Board board, PipCount[] unplacedPipCounts);
}

public class AnyRegionType : RegionType
{
    /// Determines whether the given sequence contains at
    /// least the given number of elements.
    private bool IsLengthAtLeast<T>(IEnumerable<T> source, int n) =>
        source
            .Take(n)
            .Count() == n;

    public override bool IsValid(Region region, Board board, PipCount[] unplacedPipCounts)
    {
        var pipCounts = region.GetPipCounts(board);

            // are all values equal so far?
        var equal =
            pipCounts.Length < 2
                ? true
                : pipCounts[1..]
                    .All(pipCount => pipCount == pipCounts[0]);

        return true;
    }
}

public record Region(Cell[] Cells, RegionType type)
{
    internal PipCount[] GetPipCounts(Board board) =>
        this.Cells
            .Select(cell => board[cell])
            .Where(value => value != Board.EmptyCell)
            .ToArray();
}
