namespace Pips

type Puzzle =
    {
        UnplacedDominoes : List<Domino>
        Regions : Region[]
        Board : Board
    }

module Puzzle =

    let create dominoes regions =
        let cells =
            regions
                |> Array.collect _.Cells
        let maxRow =
            cells
                |> Seq.map _.Row
                |> Seq.max
        let maxColumn =
            cells
                |> Seq.map _.Column
                |> Seq.max
        {
            UnplacedDominoes = Seq.toList dominoes
            Regions = regions
            Board = Board.create (maxRow + 1) (maxColumn + 1)
        }

    let isValid puzzle =
        puzzle.Regions
            |> Array.forall (
                Region.isValid puzzle.Board)

    let isSolved puzzle =
        puzzle.Regions
            |> Array.forall (
                Region.isSolved puzzle.Board)

    let isEmpty puzzle cell =
        Board.isEmpty puzzle.Board cell

    let rec solve puzzle =
        [
            if isValid puzzle then
                if puzzle.UnplacedDominoes.IsEmpty then
                    assert(isSolved puzzle)
                    puzzle
                else
                    match puzzle.UnplacedDominoes with
                        | domino :: rest ->
                            let cells =
                                puzzle.Regions
                                    |> Seq.collect _.Cells
                                    |> Seq.where (isEmpty puzzle)
                                    |> Seq.toArray
                            let graph = Graph.create (set cells)
                            for i = 0 to cells.Length - 2 do
                                for j = i + 1 to cells.Length - 1 do
                                    let cellA = cells[i]
                                    let cellB = cells[j]
                                    if Graph.isEdgePossible cellA cellB graph then
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
                match puzzle.Board[cell] with
                    | Board.empty -> printf "  "
                    | v -> printf $"{v} "
            printfn ""
