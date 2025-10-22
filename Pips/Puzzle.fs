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
            dominoes   // duplicate dominoes not allowed in either orientation
                |> Seq.map (fun domino ->
                    let pipCounts = Domino.toSeq domino
                    Domino.create
                        (Seq.min pipCounts)
                        (Seq.max pipCounts))
                |> Seq.distinct
                |> Seq.length = Seq.length dominoes)
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

            // gather all unplaced domino pip counts
        let unplacedPipCounts =
            UnplacedPipCounts.create [|
                for domino in puzzle.UnplacedDominoes do
                    domino.Left   // unroll Domino.toSeq for speed
                    domino.Right
            |]

            // validate each region
        puzzle.Regions
            |> Array.forall (
                Region.isValid
                    puzzle.Board
                    unplacedPipCounts)

    /// Is the given puzzle completely solved? (Note that
    /// a solved puzzle is in a valid state, but a valid
    /// puzzle might not be solved.)
    let isSolved puzzle =
        puzzle.Regions
            |> Array.forall (
                Region.isSolved puzzle.Board)

    /// Is the given cell on the given puzzle's board not
    /// covered by a domino?
    let isEmpty cell puzzle =
        Board.isEmpty cell puzzle.Board

    /// Gets all tilings for the given puzzle.
    let getAllTilings puzzle =
        puzzle.Regions
            |> Seq.collect _.Cells
            |> Seq.where (flip isEmpty puzzle)
            |> set
            |> Tiling.getAll

    /// Places the given domino in the given location in
    /// the given puzzle. This must be a valid placement.
    let place domino edge puzzle =
        let puzzle =
            {
                puzzle with
                    UnplacedDominoes =
                        puzzle.UnplacedDominoes.Remove(domino)
                    Board =
                        Board.place domino edge puzzle.Board
            }
        assert(isValid puzzle)
        puzzle

    /// Places the given domino in the given location in
    /// the given puzzle, if possible.
    let tryPlace domino edge puzzle =
        assert(isValid puzzle)

            // try to place the domino
        let puzzle =
            {
                puzzle with
                    UnplacedDominoes =
                        puzzle.UnplacedDominoes.Remove(domino)
                    Board =
                        Board.place domino edge puzzle.Board
            }

        if isValid puzzle then Some puzzle
        else None
