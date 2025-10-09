namespace Pips

/// A Pips puzzle in some state of being solved.
type Puzzle =
    {
        /// Available dominoes that have not yet been placed
        /// on the board.
        UnplacedDominoes : Set<Domino>   // assume no duplicates

        /// Regions of cells that impose constraints on the
        /// dominoes placed there.
        Regions : Region[]

        /// A board of cells, some of which may be covered
        /// with dominoes.
        Board : Board
    }

module Puzzle =

    /// Creates a puzzle in its initial state, where no dominoes
    /// have yet been placed on the board.
    let create dominoes regions =
        assert(
            Seq.distinct dominoes |> Seq.length
                = Seq.length dominoes)
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

    /// Is the given puzzle in a valid state?
    let isValid puzzle =
        puzzle.Regions
            |> Array.forall (
                Region.isValid puzzle.Board)

    /// Is the given puzzle completely solved? (Note that
    /// a solved puzzle is in a valid state, but a valid
    /// puzzle might not be solved.)
    let isSolved puzzle =
        puzzle.Regions
            |> Array.forall (
                Region.isSolved puzzle.Board)

    /// Is the given cell on the given puzzle's board not
    /// covered by a domino?
    let isEmpty puzzle cell =
        Board.isEmpty puzzle.Board cell

    /// Places the given domino in the given location in
    /// the given puzzle.
    let place domino cellLeft cellRight puzzle =
        {
            puzzle with
                UnplacedDominoes =
                    puzzle.UnplacedDominoes.Remove(domino)
                Board =
                    Board.place
                        domino cellLeft cellRight puzzle.Board
        }

    /// Finds all solutions for the given puzzle.
    let solve puzzle =

        /// Finds all solutions to the given puzzle, guided
        /// by the given possible tilings.
        let rec tile tilings puzzle =
            [
                if isValid puzzle then

                        // all dominoes have been placed successfully?
                    if puzzle.UnplacedDominoes.IsEmpty then
                        assert(isSolved puzzle)
                        puzzle
                    else
                            // try each possible tiling
                        for tiling in tilings do

                                // get edge to cover in this tiling
                            let (Node (cellA, cellB, tilings)) = tiling

                                // try each domino on that edge
                            for domino in puzzle.UnplacedDominoes do
                                yield! loop tilings domino cellA cellB puzzle
                                if domino.Left <> domino.Right then
                                    yield! loop tilings domino cellB cellA puzzle
            ]

        and loop tilings domino cellLeft cellRight puzzle =
            place domino cellLeft cellRight puzzle
                |> tile tilings

        let cells =
            puzzle.Regions
                |> Seq.collect _.Cells
                |> Seq.where (isEmpty puzzle)
                |> set

        let tilings = Tiling.getAll cells
        tile tilings puzzle

    /// Finds a arbitrary solution for the given puzzle,
    /// if at least one exists.
    let trySolve puzzle =

        let rec loop tilings puzzle =
            if isValid puzzle then
                if puzzle.UnplacedDominoes.IsEmpty then
                    assert(isSolved puzzle)
                    Some puzzle
                else
                    tilings
                        |> Seq.tryPick (fun tiling ->
                            let (Node (cellA, cellB, tilings)) = tiling
                            puzzle.UnplacedDominoes
                                |> Seq.tryPick (fun domino ->
                                    match moo tilings domino cellA cellB puzzle with
                                        | Some moo -> Some moo
                                        | None ->
                                            if domino.Left <> domino.Right then
                                                moo tilings domino cellB cellA puzzle
                                            else None))
            else None

        and moo tilings domino cellLeft cellRight puzzle =
            place domino cellLeft cellRight puzzle
                |> loop tilings

        let cells =
            puzzle.Regions
                |> Seq.collect _.Cells
                |> Seq.where (isEmpty puzzle)
                |> set

        let tilings = Tiling.getAll cells
        loop tilings puzzle
