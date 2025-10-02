namespace Pips

type PipCount = int

module PipCount =

    let minValue = 0
    let maxValue = 6

type Domino =
    {
        Left : PipCount
        Right : PipCount
    }
