namespace Pips

open Browser
open Browser.Types

open Fable.Core
open Fable.Core.JsInterop

module Canvas =

        // initialize canvas
    let canvas =
        document.getElementById "canvas"
            :?> HTMLCanvasElement

        // initialize drawing context
    let ctx = canvas.getContext_2d()

    let cellSize = 50.0

    let offset = 5.0

    let getColor region =
        match region.Type with
            | RegionType.Any -> "whitesmoke"
            | _ ->
                let range = 360
                let count = 10
                let hue = (region.GetHashCode() % (range / count)) * count
                $"hsl({hue} 100%% 80%%)"

    let drawRegion region =

        for cell in region.Cells do

            ctx.beginPath()

            let x = float cell.Column * cellSize
            let y = float cell.Row * cellSize
            ctx.rect(
                x, y,
                cellSize, cellSize)

            ctx.fillStyle <- !^(getColor region)
            ctx.fill()

            ctx.lineWidth <- 2.0
            ctx.strokeStyle <- !^"black"
            ctx.stroke()

    let drawPipCount cell (value : PipCount) =
        let x = (float cell.Column + 0.5) * cellSize
        let y = (float cell.Row + 0.5) * cellSize
        ctx.fillStyle <- !^"black"
        ctx.font <- "24px sans-serif"
        ctx.textAlign <- "center"
        ctx.textBaseline <- "middle"
        ctx.fillText(string value, x, y)

    let drawDomino (domino : Domino) ((cellA, cellB) : Edge) =

        ctx.beginPath()

        let x =
            float (min cellA.Column cellB.Column) * cellSize
        let y =
            float (min cellA.Row cellB.Row) * cellSize

        let width =
            if cellA.Row = cellB.Row then
                cellSize * 2.0
            else
                cellSize
        let height =
            if cellA.Row <> cellB.Row then
                cellSize * 2.0
            else
                cellSize

        let margin = 4.0
        ctx.rect(
            x + margin,
            y + margin,
            width - (2.0 * margin),
            height - (2.0 * margin))

        ctx.fillStyle <- !^"rgba(0, 0, 0, 0.2)"
        ctx.fill()

        ctx.lineWidth <- 1.0
        ctx.strokeStyle <- !^"black"
        ctx.stroke()

        drawPipCount cellA domino.Left
        drawPipCount cellB domino.Right

    let drawPuzzle puzzle =

        ctx.clearRect(0, 0, canvas.width, canvas.height)
        ctx.translate(offset, offset)

        for region in puzzle.Regions do
            drawRegion region

        for (domino, edge) in puzzle.Board.DominoPlaces do
            drawDomino domino edge

        ctx.setTransform(1, 0, 0, 1, 0, 0)   // resetTransform
