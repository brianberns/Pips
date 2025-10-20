namespace Pips

(*
 * ┌───────┬───────┐
 * │ ⬤ ⬤ ⬤ │ ⬤   ⬤ │
 * │       │   ⬤   │
 * │ ⬤ ⬤ ⬤ │ ⬤   ⬤ │
 * └───────┴───────┘
 *)

/// Number of pips on one side of a domino.
type PipCount = int

/// Left and right pips.
type Domino =
    {
        /// Left pips.
        Left : PipCount

        /// Right pips.
        Right : PipCount
    }

    /// Display string.
    override domino.ToString() =
        $"[{domino.Left}-{domino.Right}]"

module Domino =

    /// Creates a domino.
    let create left right =
        {
            Left = left
            Right = right
        }

    /// Is the given domino a "double", such as 6-6?
    let isDouble domino =
        domino.Left = domino.Right

    /// Converts the given domino to a sequence of pip counts.
    let toSeq domino =
        seq {
            domino.Left
            domino.Right
        }
