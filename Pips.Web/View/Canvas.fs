namespace Pips

open Browser.Types

open Fable.Core
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
            | RegionType.Any          -> ""
            | RegionType.Equal        -> "="
            | RegionType.Unequal      -> "≠"
            | RegionType.SumLess n    -> $"<{n}"
            | RegionType.SumGreater n -> $">{n}"
            | RegionType.SumExact n   -> $"{n}"

    let getConstraintColor region =
        let sat = 50
        let light = 80
        let alpha = 0.6
        match region.Type with
            | RegionType.Any          -> "whitesmoke"
            | RegionType.Equal        -> $"hsla(  0, {sat}%%, {light}%%, {alpha})"
            | RegionType.Unequal      -> $"hsla( 60, {sat}%%, {light}%%, {alpha})"
            | RegionType.SumLess _    -> $"hsla(120, {sat}%%, {light}%%, {alpha})"
            | RegionType.SumGreater _ -> $"hsla(180, {sat}%%, {light}%%, {alpha})"
            | RegionType.SumExact _   -> $"hsla(240, {sat}%%, {light}%%, {alpha})"

    let cellSize = 40.0

    let offset = 5.0

    type CanvasRenderingContext2D with

        [<Emit("($0).roundRect($1, $2, $3, $4, $5)")>]
        member this.roundRect(x: float, y: float, w: float, h: float, radii: float) : unit =
            failwith "JS interop"

    /// Draws a border line for the given cell.
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

    /// Draws the constraint string for the given region.
    let drawConstraint (ctx : Context) region =
        let cell = Seq.max region.Cells
        let x = (float cell.Column + 0.5) * cellSize
        let y = (float cell.Row + 0.5) * cellSize
        ctx.fillStyle <- !^"black"
        ctx.font <- "20px sans-serif"
        ctx.textAlign <- "center"
        ctx.textBaseline <- "middle"
        ctx.fillText(getConstraintString region, x, y)

    let outerStyle = 2.0, "black"
    let innerStyle = 1.0, "gray"

    /// Draws the given cell, including its borders.
    let drawCell (ctx : Context) regionMap fillStyle cell =

            // fill cell
        ctx.fillStyle <- !^fillStyle
        ctx.fillRect(
            float cell.Column * cellSize,
            float cell.Row * cellSize,
            cellSize,
            cellSize)

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

    /// Draws the given region by drawing each of its cells and its
    /// constraint.
    let drawRegion ctx regionMap region =
        for cell in region.Cells do
            let fillStyle = getConstraintColor region
            drawCell ctx regionMap fillStyle cell
            drawConstraint ctx region

    let drawPipCount (ctx : Context) (fontSize : string) x y (value : PipCount) =
        ctx.fillStyle <- !^"black"
        ctx.font <- $"{fontSize} sans-serif"
        ctx.textAlign <- "center"
        ctx.textBaseline <- "middle"
        ctx.fillText(string value, x, y)

    /// Draws the given unplaced domino at the given position.
    let drawUnplacedDomino (ctx : Context) x y domino =

        let scale = 2.0 / 3.0
        let cellSize = cellSize * scale

        ctx.beginPath()
        ctx.roundRect(
            x, y,
            cellSize * 2.0, cellSize,
            cellSize / 8.0)
        ctx.fillStyle <- !^"rgba(255, 255, 255, 0.9)"
        ctx.fill()
        ctx.lineWidth <- fst outerStyle * scale
        ctx.strokeStyle <- !^(snd outerStyle)
        ctx.stroke()

        ctx.beginPath()
        ctx.moveTo(x + cellSize, y)
        ctx.lineTo(x + cellSize, y + cellSize)
        ctx.lineWidth <- fst innerStyle * scale
        ctx.strokeStyle <- !^(snd innerStyle)
        ctx.stroke()

        let leftX = x + (cellSize * 0.5)
        let rightX = x + (cellSize * 1.5)
        let centerY = y + (cellSize * 0.5)
        drawPipCount ctx "18px" leftX centerY domino.Left
        drawPipCount ctx "18px" rightX centerY domino.Right

    /// Draws the given unplaced dominoes starting at the given
    /// Y position.
    let drawUnplacedDominoes (ctx : Context) startY dominoes =
        let dominoChunks =
            dominoes
                |> Seq.chunkBySize 4 
                |> Seq.toArray
        for row = 0 to dominoChunks.Length - 1 do
            let dominoChunk = dominoChunks[row]
            for col = 0 to dominoChunk.Length - 1 do
                let domino = dominoChunk[col]
                let x = float col * cellSize * 2.0
                let y = startY + (float row * cellSize)
                drawUnplacedDomino ctx x y domino

    /// Draws the given puzzle by drawing its regions and unplaced
    /// dominoes.
    let drawPuzzle (ctx : Context) puzzle =

        let regionMap = getRegionMap puzzle

        ctx.clearRect(0, 0, ctx.canvas.width, ctx.canvas.height)
        ctx.translate(offset, offset)

        for region in puzzle.Regions do
            drawRegion ctx regionMap region

        let startY = float (puzzle.Board.NumRows + 1) * cellSize
        drawUnplacedDominoes ctx startY puzzle.UnplacedDominoes

        ctx.setTransform(1, 0, 0, 1, 0, 0)   // resetTransform

    let drawSolutionPipCount ctx cell pipCount =
        let x = (float cell.Column + 0.5) * cellSize
        let y = (float cell.Row + 0.5) * cellSize
        drawPipCount ctx "24px" x y pipCount

    let drawSolutionDomino (ctx : Context) domino ((cellA, cellB) : Edge) =

        ctx.beginPath()

        let x =
            float (min cellA.Column cellB.Column) * cellSize
        let y =
            float (min cellA.Row cellB.Row) * cellSize

        let x, width =
            if cellA.Row = cellB.Row then
                x, cellSize * 2.0
            else
                x, cellSize
        let y, height =
            if cellA.Row <> cellB.Row then
                y, cellSize * 2.0
            else
                y, cellSize
        ctx.roundRect(
            x, y,
            width, height,
            cellSize / 8.0)
        ctx.lineWidth <- fst outerStyle
        ctx.strokeStyle <- !^(snd outerStyle)
        ctx.stroke()

        drawSolutionPipCount ctx cellA domino.Left
        drawSolutionPipCount ctx cellB domino.Right

    let drawSolutions (ctx : Context) (solutions : _[]) =

        ctx.clearRect(0, 0, ctx.canvas.width, ctx.canvas.height)
        ctx.translate(offset, offset)

        let solution = Seq.head solutions
        for (domino, edge) in solution.Board.DominoPlaces do
            drawSolutionDomino ctx domino edge

        ctx.setTransform(1, 0, 0, 1, 0, 0)   // resetTransform
