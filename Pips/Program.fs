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

type Board = Map<Cell, Value>

type Region =
    {
        Cells : Cell[]
        Type : Constraint
    }

module Region =

    let getValues (board : Board) region =
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
        Board : Board
    }

module Puzzle =

    let isSolved puzzle =
        puzzle.Regions
            |> Array.forall (
                Region.isSolved puzzle.Board)

    let private allCells puzzle =
        puzzle.Regions
        |> Array.collect (fun region -> region.Cells)

    let rec solve puzzle : Set<Board> =
        if isSolved puzzle then
            Set.singleton puzzle.Board
        else
            match puzzle.UnplacedDominoes with
            | [] -> Set.empty
            | domino :: rest ->
                allCells puzzle
                |> Array.fold (
                    fun solutions cell1 ->
                        allCells puzzle
                        |> Array.fold (
                            fun solutions cell2 ->
                                if Cell.adjacent cell1 cell2 && not (Map.containsKey cell1 puzzle.Board) && not (Map.containsKey cell2 puzzle.Board) then
                                    let newBoard =
                                        puzzle.Board
                                        |> Map.add cell1 domino.Left
                                        |> Map.add cell2 domino.Right

                                    let newPuzzle =
                                        {
                                            puzzle with
                                                UnplacedDominoes = rest
                                                Board = newBoard
                                        }

                                    let solutionsAfterFirstOrientation = Set.union solutions (solve newPuzzle)

                                    let newBoardReversed = 
                                        puzzle.Board
                                        |> Map.add cell1 domino.Right
                                        |> Map.add cell2 domino.Left

                                    let newPuzzleReversed = 
                                        { puzzle with
                                            UnplacedDominoes = rest
                                            Board = newBoardReversed
                                        }

                                    Set.union solutionsAfterFirstOrientation (solve newPuzzleReversed)
                                else
                                    solutions
                        ) solutions
                ) Set.empty

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


let print boards =
    boards
    |> Set.iter (fun board ->
        let maxRow = puzzle.Regions |> Array.collect (fun r -> r.Cells) |> Array.map (fun c -> c.Row) |> Array.max
        let maxCol = puzzle.Regions |> Array.collect (fun r -> r.Cells) |> Array.map (fun c -> c.Column) |> Array.max
        for r in 0..maxRow do
            for c in 0..maxCol do
                match Map.tryFind { Row = r; Column = c } board with
                | Some v -> printf $"{v} "
                | None -> printf "  "
            printfn ""
    )

print (Puzzle.solve puzzle)
