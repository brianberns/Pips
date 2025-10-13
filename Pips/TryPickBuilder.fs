namespace Pips

/// Computation builder that short-circuits on the first Some
/// value in a `for` loop.
type TryPickBuilder() =
    member _.Bind(opt, f) = Option.bind f opt
    member _.Return(x) = Some x
    member _.ReturnFrom(opt : Option<_>) = opt
    member _.Zero() = None
    member _.Yield(x) = Some x
    member _.YieldFrom(opt : Option<_>) = opt
    member _.Delay(f) = f
    member _.Run(f) = f()

    /// Combines multiple yields by evaluating the second one
    /// iff the first one fails.
    member _.Combine(opt, thunk) =
        Option.orElseWith thunk opt

    /// Short-circuit `for` loop.
    member _.For(seq, body) =
        Seq.tryPick body seq

[<AutoOpen>]
module TryPickBuilder =

    /// Computation builder that short-circuits on the first
    /// Some value in a for loop.
    let tryPick = TryPickBuilder()
