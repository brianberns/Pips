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

/// The two sides of a domino.
type Domino =
    {
        /// Left side of the domino.
        Left : PipCount

        /// Right side of the domino.
        Right : PipCount
    }

    /// Display string.
    override domino.ToString() =
        $"[{domino.Left}-{domino.Right}]"

(*
 * Note that an X-Y domino is technically not equal to a Y-X
 * domino by this definition, but since Pips puzzles never
 * contain duplicate dominoes, we don't need to account for
 * this via custom equality or comparison.
 *)

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
