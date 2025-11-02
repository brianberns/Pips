namespace PipsCsharp;

/// Number of pips on one side of a domino.
using PipCount = int;

/// The two sides of a domino.
public record Domino(PipCount Left, PipCount Right)
{
    /// Display string.
    public override string ToString() =>
        $"[{Left}-{Right}]";

    /// Is the given domino a "double", such as 6-6?
    public bool IsDouble =>
        Left == Right;

    /// Converts the given domino to a sequence of pip counts.
    public IEnumerable<PipCount> PipCounts
    {
        get
        {
            yield return Left;
            yield return Right;
        }
    }
}
