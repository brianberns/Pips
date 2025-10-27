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
                $"hsl({hue} 100%% 60%%)"

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

            ctx.strokeStyle <- !^"black"
            ctx.stroke()

    let drawPuzzle puzzle =

        ctx.clearRect(0, 0, canvas.width, canvas.height)
        ctx.translate(offset, offset)
        ctx.lineWidth <- 2.0

        for region in puzzle.Regions do
            drawRegion region

        ctx.setTransform(1, 0, 0, 1, 0, 0)   // resetTransform
