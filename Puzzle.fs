namespace Pips

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

    let isEmpty puzzle cell =
        Board.isEmpty puzzle.Board cell

    let rec solve puzzle =
        [
            if isSolved puzzle then
                puzzle
            else
                match puzzle.UnplacedDominoes with
                    | domino :: rest ->
                        let cells =
                            puzzle.Regions
                                |> Seq.collect _.Cells
                                |> Seq.where (isEmpty puzzle)
                                |> Seq.toArray
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

    let printBoard puzzle =
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
        for row in 0 .. maxRow do
            for col in 0 .. maxCol do
                let cell = { Row = row; Column = col }
                match Board.tryGetValue cell puzzle.Board with
                    | Some v -> printf $"{v} "
                    | None -> printf "  "
            printfn ""
