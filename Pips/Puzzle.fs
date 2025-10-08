namespace Pips

type Puzzle =
    {
        UnplacedDominoes : Set<Domino>   // assume no duplicates
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
            UnplacedDominoes = set dominoes
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

    let solve puzzle =

        let rec loop tilings puzzle =
            [
                if isValid puzzle then
                    if puzzle.UnplacedDominoes.IsEmpty then
                        assert(isSolved puzzle)
                        puzzle
                    else
                        for tiling in tilings do
                            let (Node ((cellA, cellB), tilings)) = tiling
                            for domino in puzzle.UnplacedDominoes do
                                yield! place domino tilings cellA cellB puzzle
                                if domino.Left <> domino.Right then
                                    yield! place domino tilings cellB cellA puzzle
            ]

        and place domino tiling cellLeft cellRight puzzle =
            loop tiling {
                puzzle with
                    UnplacedDominoes =
                        puzzle.UnplacedDominoes.Remove(domino)
                    Board =
                        Board.place
                            domino cellLeft cellRight puzzle.Board
            }

        let cells =
            puzzle.Regions
                |> Seq.collect _.Cells
                |> Seq.where (isEmpty puzzle)
                |> set

        let tilings = Tiling.getAll cells
        loop tilings puzzle

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
