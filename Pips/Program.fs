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

type Board =
    {
        Dominoes : Set<Domino * Cell * Cell>
        CellMap : Map<Cell, Value>
    }

module Board =

    let empty =
        {
            Dominoes = Set.empty
            CellMap = Map.empty
        }

    let place domino cellLeft cellRight board =
        assert(Cell.adjacent cellLeft cellRight)
        assert(not (Map.containsKey cellLeft board.CellMap))
        assert(not (Map.containsKey cellRight board.CellMap))
        {
            CellMap =
                board.CellMap
                    |> Map.add cellLeft domino.Left
                    |> Map.add cellRight domino.Right
            Dominoes =
                board.Dominoes
                    |> Set.add (domino, cellLeft, cellRight)
        }

    let tryGetValue cell board =
        board.CellMap
            |> Map.tryFind cell

type Region =
    {
        Cells : Cell[]
        Type : Constraint
    }

module Region =

    let allCells regions =
        let cells =
            Array.collect _.Cells regions
        assert(
            cells
                |> Seq.distinct
                |> Seq.length
                    = cells.Length)
        cells

    let getValues board region =
        region.Cells
            |> Array.choose (fun cell ->
                Board.tryGetValue cell board)

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
        Board : Board
    }

module Puzzle =

    let isSolved puzzle =
        puzzle.Regions
            |> Array.forall (
                Region.isSolved puzzle.Board)

    let isEmpty cell puzzle =
        Board.tryGetValue cell puzzle.Board
            |> Option.isNone

    let rec solve puzzle =
        [
            if isSolved puzzle then
                puzzle.Board
            else
                match puzzle.UnplacedDominoes with
                    | domino :: rest ->
                        let cells =
                            Region.allCells puzzle.Regions
                                |> Array.where (fun cell ->
                                    isEmpty cell puzzle)
                        let pairs =
                            seq {
                                for i = 0 to cells.Length - 2 do
                                    for j = 1 to cells.Length - 1 do
                                        cells[i], cells[j]
                            }
                        for (cellA, cellB) in pairs do
                            if Cell.adjacent cellA cellB then
                                yield! loop domino rest cellA cellB puzzle
                                if domino.Left <> domino.Right then
                                    yield! loop domino rest cellB cellA puzzle
                    | [] -> ()
        ]

    and loop domino rest cellLeft cellRight puzzle =
        solve {
            puzzle with
                UnplacedDominoes = rest
                Board =
                    Board.place
                        domino cellLeft cellRight puzzle.Board
        }

    let printBoard board puzzle =
        let maxRow =
            puzzle.Regions
                |> Array.collect _.Cells
                |> Array.map _.Row
                |> Array.max
        let maxCol =
            puzzle.Regions
                |> Array.collect _.Cells
                |> Array.map _.Column
                |> Array.max
        for r in 0 .. maxRow do
            for c in 0 .. maxCol do
                match Board.tryGetValue { Row = r; Column = c } board with
                    | Some v -> printf $"{v} "
                    | None -> printf "  "
            printfn ""

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
        Board = Board.empty
    }

let print boards =
    for board in boards do
        Puzzle.printBoard board puzzle
        printfn ""

print (Puzzle.solve puzzle)
