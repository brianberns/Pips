namespace Pips.Web

open Fable.Core.JsInterop
open Pips

module Region =

    /// Are the given cell's in the same region?
    let private inSameRegion regionMap cellA cellB =
        Map.tryFind cellA regionMap
            = Map.tryFind cellB regionMap

    /// Does the given cell have a left border?
    let private hasLeftBorder regionMap cell =
        let adj = { cell with Column = cell.Column - 1 }
        not (inSameRegion regionMap cell adj)

    /// Does the given cell have a right border?
    let private hasRightBorder regionMap cell =
        let adj = { cell with Column = cell.Column + 1 }
        not (inSameRegion regionMap cell adj)

    /// Does the given cell have a top border?
    let private hasTopBorder regionMap cell =
        let adj = { cell with Row = cell.Row - 1 }
        not (inSameRegion regionMap cell adj)

    /// Does the given cell have a bottom border?
    let private hasBottomBorder regionMap cell =
        let adj = { cell with Row = cell.Row + 1 }
        not (inSameRegion regionMap cell adj)

    /// Constraint display string.
    let private getConstraintString region =
        match region.Type with
            | RegionType.Any          -> ""
            | RegionType.Equal        -> "="
            | RegionType.Unequal      -> "≠"
            | RegionType.SumLess n    -> $"<{n}"
            | RegionType.SumGreater n -> $">{n}"
            | RegionType.SumExact n   -> $"{n}"

    /// Constraint color.
    let private getConstraintColor region =
        let level =
            match region.Type with
                | RegionType.Any          -> 250
                | RegionType.Equal        -> 235
                | RegionType.Unequal      -> 220
                | RegionType.SumLess _    -> 205
                | RegionType.SumGreater _ -> 190
                | RegionType.SumExact _   -> 175
        $"rgb({level}, {level}, {level})"

    /// Length of one side of a cell.
    let private cellSize = Domino.cellSize

    /// Draws a border line for the given cell.
    let private drawBorder
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
    let private drawConstraint (ctx : Context) region =
        let cell = Seq.max region.Cells
        let x = (float cell.Column + 0.5) * cellSize
        let y = (float cell.Row + 0.5) * cellSize
        let fontSize = int (Domino.cellSize / 2.2)
        ctx.fillStyle <- !^"black"
        ctx.font <- $"{fontSize}px sans-serif"
        ctx.textAlign <- "center"
        ctx.textBaseline <- "middle"
        ctx.fillText(getConstraintString region, x, y)

    /// Outer border of a region.
    let outerStyle = Domino.outerStyle

    /// Inner cell divider in a region.
    let innerStyle = Domino.innerStyle

    /// Draws the given cell, including its borders.
    let private drawCell (ctx : Context) regionMap fillStyle cell =

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
