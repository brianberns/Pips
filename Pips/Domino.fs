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

module PipCount =

    /// Smallest pip count.
    let minValue = 0

    /// Largest pip count.
    let maxValue = 6

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