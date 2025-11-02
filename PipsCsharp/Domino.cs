namespace PipsCsharp;

/// Number of pips on one side of a domino.
using PipCount = int;

/// The two sides of a domino.
public class Domino(PipCount left, PipCount right)
{
    /// Left side of the domino.
    public PipCount Left { get; } = left;

    /// Right side of the domino.
    public PipCount Right { get; } = right;

    /// Display string.
    public override string ToString() =>
        $"[{this.Left}-{this.Right}]";

    /// Is the given domino a "double", such as 6-6?
    public bool IsDouble =>
        this.Left == this.Right;

    /// Converts the given domino to a sequence of pip counts.
    IEnumerable<PipCount> PipCounts
    {
        get
        {
            yield return this.Left;
            yield return this.Right;
        }
    }
}
