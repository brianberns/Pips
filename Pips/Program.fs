type Value = int

type Domino =
    {
        Left : Value
        Right : Value
    }

type Constraint =
    | Unconstrained
    | Equal
    | Unequal
    | SumLess of int
    | SumGreater of int
    | Sum of int

type Cell =
    {
        Row : int
        Column : int
    }

module Cell =

    let adjacent cellA cellB =
        (cellA.Row = cellB.Row
            && abs (cellA.Column - cellB.Column) = 1)
            || (cellA.Column = cellB.Column
                && abs (cellA.Row - cellB.Row) = 1)

type Region =
    {
        Cells : Cell[]
        Type : Constraint
    }

module Region =

    let getValues board region =
        region.Cells
            |> Array.choose (fun cell ->
                Map.tryFind cell board)

    let tryGetValues board region =
        let values = getValues board region
        if values.Length = region.Cells.Length then
            Some values
        else
            None

    let isSolved board region =
        match tryGetValues board region, region.Type with
            | None, _ -> false
            | Some _, Unconstrained -> true
            | Some values, Equal ->
                (Array.distinct values).Length = 1
            | Some values, Unequal ->
                (Array.distinct values).Length = values.Length
            | Some values, SumLess n ->
                Array.sum values < n
            | Some values, SumGreater n ->
                Array.sum values > n
            | Some values, Sum n ->
                Array.sum values = n

type Puzzle =
    {
        UnplacedDominoes : List<Domino>
        Regions : Region[]
        Board : Map<Cell, Value>
    }

module Puzzle =

    let isSolved puzzle =
        puzzle.Regions
            |> Array.forall (
                Region.isSolved puzzle.Board)

    let rec solve puzzle =
        if isSolved puzzle then
            Set.singleton puzzle
        else
            match puzzle.UnplacedDominoes with
                | [] -> Set.empty
                | domino :: rest ->
                    Set.empty

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
                    Type = SumGreater 3
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
                    Type = Unconstrained
                }
                {
                    Cells = [|
                        { Row = 1; Column = 3 } |]
                    Type = SumLess 3
                }
                {
                    Cells = [|
                        { Row = 2; Column = 1 } |]
                    Type = SumGreater 4
                }
            |]
        Board = Map.empty
    }

printfn $"{Puzzle.solve puzzle}"
