namespace Pips

open Browser.Types
open Fable.Core.JsInterop

type Context = CanvasRenderingContext2D

module Canvas =

    let getRegionMap puzzle =
        Map [
            for region in puzzle.Regions do
                for cell in region.Cells do
                    yield cell, region
        ]

    let inSameRegion regionMap c1 c2 =
        Map.tryFind c1 regionMap = Map.tryFind c2 regionMap

    let hasLeftBorder regionMap cell =
        let adj = { cell with Column = cell.Column - 1 }
        not (inSameRegion regionMap cell adj)

    let hasRightBorder regionMap cell =
        let adj = { cell with Column = cell.Column + 1 }
        not (inSameRegion regionMap cell adj)

    let hasTopBorder regionMap cell =
        let adj = { cell with Row = cell.Row - 1 }
        not (inSameRegion regionMap cell adj)

    let hasBottomBorder regionMap cell =
        let adj = { cell with Row = cell.Row + 1 }
        not (inSameRegion regionMap cell adj)

    let getConstraintString region =
        match region.Type with
            | RegionType.Any -> "✶"
            | RegionType.Equal -> "="
            | RegionType.Unequal -> "≠"
            | RegionType.SumLess n -> $"<{n}"
            | RegionType.SumGreater n -> $">{n}"
            | RegionType.SumExact n -> $"{n}"

    let cellSize = 40.0

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

    let drawConstraint (ctx : Context) region =
        let cell = Seq.max region.Cells
        let x = (float cell.Column + 0.5) * cellSize
        let y = (float cell.Row + 0.5) * cellSize
        ctx.fillStyle <- !^"black"
        ctx.font <- "20px sans-serif"
        ctx.textAlign <- "center"
        ctx.textBaseline <- "middle"
        ctx.fillText(getConstraintString region, x, y)

    let drawRegion (ctx : Context) regionMap outerStyle innerStyle region =
        for cell in region.Cells do

                // draw left border?
            if hasLeftBorder regionMap cell then
                drawBorder ctx cell (0, 0) (1, 0) outerStyle

                // draw right border
            let style =
                if hasRightBorder regionMap cell then outerStyle
                else innerStyle
            drawBorder ctx cell (0, 1) (1, 1) style

                // draw top border?
            if hasTopBorder regionMap cell then
                drawBorder ctx cell (0, 0) (0, 1) outerStyle

                // draw bottom border
            let style =
                if hasBottomBorder regionMap cell then outerStyle
                else innerStyle
            drawBorder ctx cell (1, 0) (1, 1) style

                // draw constraint
            drawConstraint ctx region

    let drawPuzzle (ctx : Context) puzzle =

        let regionMap = getRegionMap puzzle

        ctx.clearRect(0, 0, ctx.canvas.width, ctx.canvas.height)
        ctx.translate(offset, offset)

        let outerStyle = 2.0, "black"
        let innerStyle = 1.0, "lightgray"
        for region in puzzle.Regions do
            drawRegion ctx regionMap outerStyle innerStyle region

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
