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
    let private offset = 10.0

    /// Draws the given puzzle by drawing its regions and unplaced
    /// dominoes.
    let drawPuzzle (ctx : Context) puzzle =

        let regionMap = getRegionMap puzzle

        ctx.translate(offset, offset)

        for region in puzzle.Regions do
            Region.drawRegion ctx regionMap region

        let startY = float (puzzle.Board.NumRows + 1) * cellSize
        Domino.drawUnplacedDominoes ctx startY puzzle.UnplacedDominoes

        ctx.setTransform(1, 0, 0, 1, 0, 0)   // resetTransform

    /// Draws the given solutions.
    let drawSolutions (ctx : Context) (solutions : _[]) =

        let callback iFrame =

            Canvas.clear ctx
            ctx.translate(offset, offset)

            let solution = solutions[iFrame % solutions.Length]
            for (domino, edge) in solution.Board.DominoPlaces do
                Domino.drawSolutionDomino ctx domino edge

            ctx.setTransform(1, 0, 0, 1, 0, 0)   // resetTransform

        Canvas.animate 2.0 callback
