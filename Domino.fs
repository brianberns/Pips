namespace Pips

type PipCount = int

module PipCount =

    let minValue = 0
    let maxValue = 6

[<Struct>]
type Domino =
    {
        Left : PipCount
        Right : PipCount
    }
