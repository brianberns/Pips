namespace Pips.Web

open Pips

module Puzzle =

    let private getRegionMap puzzle =
        Map [
            for region in puzzle.Regions do
                for cell in region.Cells do
                    yield cell, region
        ]

    let private cellSize = Domino.cellSize

    /// Draws the given puzzle by drawing its regions and unplaced
    /// dominoes.
    let drawPuzzle (ctx : Context) puzzle =

        let regionMap = getRegionMap puzzle

        for region in puzzle.Regions do
            Region.drawRegion ctx regionMap region

        let startY = float (puzzle.Board.NumRows + 1) * cellSize
        Domino.drawUnplacedDominoes ctx startY puzzle.UnplacedDominoes

    /// Draws the given solutions.
    let drawSolutions (ctx : Context) (solutions : _[]) =

        let callback iFrame =

            Canvas.clear ctx

            let solution = solutions[iFrame % solutions.Length]
            for (domino, edge) in solution.Board.DominoPlaces do
                Domino.drawSolutionDomino ctx domino edge

        Canvas.animate 10.0 callback
