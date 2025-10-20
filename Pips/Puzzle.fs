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

        /// Map from cells to their containing regions.
        RegionMap : Map<Cell, int (*region index*)>

        /// A board of cells, some of which may be covered
        /// with dominoes.
        Board : Board
    }

module Puzzle =

    /// Maps the given regions' cells.
    let getRegionMap (regions : _[]) =
        Map [
            for iRegion = 0 to regions.Length - 1 do
                for cell in regions[iRegion].Cells do
                    cell, iRegion
        ]

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
            RegionMap = getRegionMap regions
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

    /// Tries to place the given domino in the given location in
    /// the given puzzle.
    let tryPlace domino ((cellA, cellB) as edge) puzzle =
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

            // affected regions are still valid?
        let regionIdA = puzzle.RegionMap[cellA]
        let regionIdB = puzzle.RegionMap[cellB]
        if regionIdA = regionIdB then
            let region = puzzle.Regions[regionIdA]
            if Region.isValid puzzle.Board region then
                assert(isValid puzzle)
                Some puzzle
            else None
        else
            let regionA = puzzle.Regions[regionIdA]
            let regionB = puzzle.Regions[regionIdB]
            if Region.isValid puzzle.Board regionA
                && Region.isValid puzzle.Board regionB then
                assert(isValid puzzle)
                Some puzzle
            else None
