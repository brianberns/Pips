namespace Pips

open Browser
open Browser.Types

open Fable.Core.JsInterop

module Canvas =

        // initialize canvas
    let canvas =
        document.getElementById "canvas"
            :?> HTMLCanvasElement

        // initialize drawing context
    let ctx = canvas.getContext_2d()

    let cellSize = 50.0

    let drawRegion region =
        for cell in region.Cells do
            let x = float cell.Column * cellSize
            let y = float cell.Row * cellSize
            ctx.beginPath()
            ctx.rect(x, y, cellSize, cellSize)
            ctx.fillStyle <- !^"white"
            ctx.fill()
            ctx.strokeStyle <- !^"black"
            ctx.stroke()

    let drawPuzzle puzzle =
        ctx.clearRect(0, 0, canvas.width, canvas.height)
        for region in puzzle.Regions do
            drawRegion region
