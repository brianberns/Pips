namespace Pips

open Browser.Types
open Fable.Core.JsInterop

type Context = CanvasRenderingContext2D

module Canvas =

    let cellSize = 50.0

    let offset = 5.0

    let drawRegion (ctx : Context) region =

        for cell in region.Cells do

            ctx.beginPath()

            let x = float cell.Column * cellSize
            let y = float cell.Row * cellSize
            ctx.rect(
                x, y,
                cellSize, cellSize)

            ctx.fillStyle <- !^"white"
            ctx.fill()

            ctx.lineWidth <- 2.0
            ctx.strokeStyle <- !^"black"
            ctx.stroke()

    let drawPuzzle (ctx : Context) puzzle =

        let cells =
            puzzle.Regions
                |> Seq.collect _.Cells
                |> set

        let maxRow =
            if Seq.isEmpty cells then 0
            else
                cells
                    |> Seq.map _.Row
                    |> Seq.max

        let maxCol =
            if Seq.isEmpty cells then 0
            else
                cells
                    |> Seq.map _.Column
                    |> Seq.max

        let regionMap =
            Map [
                for region in puzzle.Regions do
                    for cell in region.Cells do
                        yield cell, region
            ]

        let inSameRegion c1 c2 =
            Map.tryFind c1 regionMap = Map.tryFind c2 regionMap

        let isPresent cell =
            cells.Contains(cell)

        let hasHorizontalRegionBorder row col =
            let cell = Cell.create row col
            let topCell = Cell.create (row - 1) cell.Column
            isPresent cell && isPresent topCell
                && not (inSameRegion cell topCell)

        let hasVerticalRegionBorder row col =
            let cell = Cell.create row col
            let leftCell = Cell.create cell.Row (col - 1)
            (not (isPresent cell) || not (isPresent leftCell))
                && not (inSameRegion cell leftCell)

        ctx.clearRect(0, 0, ctx.canvas.width, ctx.canvas.height)
        ctx.translate(offset, offset)

        for region in puzzle.Regions do
            drawRegion ctx region

        ctx.setTransform(1, 0, 0, 1, 0, 0)   // resetTransform

    let drawPipCount (ctx : Context) cell (value : PipCount) =
        let x = (float cell.Column + 0.5) * cellSize
        let y = (float cell.Row + 0.5) * cellSize
        ctx.fillStyle <- !^"black"
        ctx.font <- "24px sans-serif"
        ctx.textAlign <- "center"
        ctx.textBaseline <- "middle"
        ctx.fillText(string value, x, y)

    let drawDomino (ctx : Context) domino ((cellA, cellB) : Edge) =

        ctx.beginPath()

        let x =
            float (min cellA.Column cellB.Column) * cellSize
        let y =
            float (min cellA.Row cellB.Row) * cellSize

        let margin = 4.0
        let dominoHalfSize = cellSize - (2.0 * margin)
        let x, width =
            if cellA.Row = cellB.Row then
                x + margin, dominoHalfSize * 2.0
            else
                x, dominoHalfSize
        let y, height =
            if cellA.Row <> cellB.Row then
                y + margin, dominoHalfSize * 2.0
            else
                y, dominoHalfSize
        ctx.rect(
            x + margin, y + margin,
            width, height)

        ctx.fillStyle <- !^"rgba(255, 255, 255, 0.9)"
        ctx.fill()

        ctx.lineWidth <- 1.0
        ctx.strokeStyle <- !^"black"
        ctx.stroke()

        drawPipCount ctx cellA domino.Left
        drawPipCount ctx cellB domino.Right
