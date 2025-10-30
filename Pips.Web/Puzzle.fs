namespace Pips.Web

open Pips

module Puzzle =

    /// Draws the given puzzle's regions.
    let drawPuzzle ctx puzzle =
        let regionMap =
            Map [
                for region in puzzle.Regions do
                    for cell in region.Cells do
                        yield cell, region
            ]
        for region in puzzle.Regions do
            Region.drawRegion ctx regionMap region

    /// Draws the given puzzle's unplaced dominoes.
    let drawUnplacedDominoes ctx chunkSize puzzle =
        let dominoChunks =
            puzzle.UnplacedDominoes
                |> Seq.chunkBySize chunkSize
                |> Seq.toArray
        for row = 0 to dominoChunks.Length - 1 do
            let dominoChunk = dominoChunks[row]
            for col = 0 to dominoChunk.Length - 1 do
                let domino = dominoChunk[col]
                let x = float col * Domino.cellSize * 2.0
                let y = float row * Domino.cellSize
                Domino.drawUnplacedDomino ctx x y domino

    /// Draws the given solutions.
    let drawSolutions ctx (solutions : _[]) =

        let callback iFrame =

            Canvas.clear ctx

            let solution = solutions[iFrame % solutions.Length]
            for (domino, edge) in solution.Board.DominoPlaces do
                Domino.drawSolutionDomino ctx domino edge

        Canvas.animate 10.0 callback
