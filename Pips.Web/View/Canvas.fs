namespace Pips

open Browser.Types
open Fable.Core.JsInterop

type Context = CanvasRenderingContext2D

module Canvas =

    let cellSize = 50.0

    let offset = 5.0

    let drawBorder
        (ctx : Context)
        cell
        (rowFrom, colFrom)
        (rowTo, colTo)
        (lineWidth, strokeStyle : string) =

        let xFrom = float (cell.Column + colFrom) * cellSize
        let yFrom = float (cell.Row + rowFrom) * cellSize
        let xTo = float (cell.Column + colTo) * cellSize
        let yTo = float (cell.Row + rowTo) * cellSize

        ctx.beginPath()
        ctx.moveTo(xFrom, yFrom)
        ctx.lineTo(xTo, yTo)
        ctx.lineWidth <- lineWidth
        ctx.strokeStyle <- !^strokeStyle
        ctx.stroke()

    let drawPuzzle (ctx : Context) puzzle =

        let cells =
            puzzle.Regions
                |> Seq.collect _.Cells
                |> set

        let regionMap =
            Map [
                for region in puzzle.Regions do
                    for cell in region.Cells do
                        yield cell, region
            ]

        let inSameRegion c1 c2 =
            Map.tryFind c1 regionMap = Map.tryFind c2 regionMap

        let hasLeftRegionBorder cell =
            let adj = { cell with Column = cell.Column - 1 }
            not (inSameRegion cell adj)

        let hasRightRegionBorder cell =
            let adj = { cell with Column = cell.Column + 1 }
            not (inSameRegion cell adj)

        let hasTopRegionBorder cell =
            let adj = { cell with Row = cell.Row - 1 }
            not (inSameRegion cell adj)

        let hasBottomRegionBorder cell =
            let adj = { cell with Row = cell.Row + 1 }
            not (inSameRegion cell adj)

        ctx.clearRect(0, 0, ctx.canvas.width, ctx.canvas.height)
        ctx.translate(offset, offset)

        let outerStyle = 2.0, "black"
        let innerStyle = 1.0, "lightgray"

        for cell in cells do

                // draw left border?
            if hasLeftRegionBorder cell then
                drawBorder ctx cell (0, 0) (1, 0) outerStyle

                // draw right border
            let style =
                if hasRightRegionBorder cell then outerStyle
                else innerStyle
            drawBorder ctx cell (0, 1) (1, 1) style

                // draw top border?
            if hasTopRegionBorder cell then
                drawBorder ctx cell (0, 0) (0, 1) outerStyle

                // draw bottom border
            let style =
                if hasBottomRegionBorder cell then outerStyle
                else innerStyle
            drawBorder ctx cell (1, 0) (1, 1) style

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
