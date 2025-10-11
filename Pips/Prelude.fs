namespace Pips

[<AutoOpen>]
module Prelude =

    /// Uncurries the given function.
    let uncurry f (a, b) = f a b

    /// Flips the argument order for the given function.
    let flip f a b = f b a
