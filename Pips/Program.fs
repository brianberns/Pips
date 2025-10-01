type Value = int

type Domino =
    {
        Left : Value
        Right : Value
    }

type RegionType =
    | Empty
    | Equal
    | Unequal
    | Less of int
    | Greater of int
    | Sum of int

type Cell =
    {
        Row : int
        Column : int
    }

type Region =
    {
        Cells : Cell[]
        Type : RegionType
    }

type Puzzle =
    {
        UnplacedDominoes : List<Domino>
        Regions : Region[]
        Board : Map<Cell, Value>
    }

let puzzle =
    {
        UnplacedDominoes =
            [
                { Left = 4; Right = 4 }
                { Left = 3; Right = 5 }
                { Left = 0; Right = 3 }
                { Left = 2; Right = 2 }
            ]
        Regions =
            [|
                {
                    Cells = [|
                        { Row = 0; Column = 1 } |]
                    Type = Greater 3
                }
                {
                    Cells = [|
                        { Row = 0; Column = 2 }
                        { Row = 1; Column = 1 }
                        { Row = 1; Column = 2 }
                        { Row = 2; Column = 2 } |]
                    Type = Unequal
                }
                {
                    Cells = [|
                        { Row = 1; Column = 0 } |]
                    Type = Empty
                }
                {
                    Cells = [|
                        { Row = 1; Column = 3 } |]
                    Type = Less 3
                }
                {
                    Cells = [|
                        { Row = 2; Column = 1 } |]
                    Type = Greater 4
                }
            |]
        Board = Map.empty
    }

printfn $"{puzzle}"
