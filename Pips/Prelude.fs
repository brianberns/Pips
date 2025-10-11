namespace Pips

[<AutoOpen>]
module Prelude =

    let uncurry f (a, b) = f a b

    let flip f a b = f b a
