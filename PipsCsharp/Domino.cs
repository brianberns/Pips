namespace PipsCsharp;

/// Number of pips on one side of a domino.
public record PipCount(int Value);

/// The two sides of a domino.
public record Domino(PipCount Left, PipCount Right)
{
    /// Display string.
    public override string ToString() =>
        $"[{Left}-{Right}]";

    /// Is this domino a "double", such as 6-6?
    public bool IsDouble =>
        Left == Right;

    /// Converts this domino to a sequence of pip counts.
    public IEnumerable<PipCount> PipCounts
    {
        get
        {
            yield return Left;
            yield return Right;
        }
    }
}
