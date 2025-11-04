namespace PipsCsharp;

public abstract class RegionType
{
    public abstract bool IsValid(Region region, Board board, PipCount[] unplacedPipCounts);
}

public static class EnumerableExt
{
    /// Determines whether the given sequence contains at
    /// least the given number of elements.
    public static bool IsLengthAtLeast<T>(this IEnumerable<T> source, int n) =>
        source
            .Take(n)
            .Count() == n;
}

/// Cells in the region can have any value.
public class AnyRegionType : RegionType
{
    public override bool IsValid(Region region, Board board, PipCount[] unplacedPipCounts)
    {
        return true;
    }
}

/// All cells in the region must have the same value.
public class EqualRegionType : RegionType
{
    /// Validates an Equal region.
    public override bool IsValid(Region region, Board board, PipCount[] unplacedPipCounts)
    {
        var pipCounts = region.GetPipCounts(board);

            // are all values equal so far?
        var equal =
            pipCounts.Length < 2
                ? true
                : pipCounts[1..]
                    .All(pipCount => pipCount == pipCounts[0]);

            // are there enough matching values available?
        if (equal && pipCounts.Length > 0)
        {
            var value = pipCounts[0];
            var nNeeded =
                region.Cells.Length - pipCounts.Length;
            return unplacedPipCounts
                .Where(pipCount => pipCount == value)
                .IsLengthAtLeast(nNeeded);
        }
        else
        {
            return equal;
        }
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
