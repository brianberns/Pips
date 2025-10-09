namespace Pips

(*
 * ┌───────┬───────┐
 * │ o o o │ o   o │
 * │       │   o   │
 * │ o o o │ o   o │
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
