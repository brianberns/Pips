namespace Pips

open Browser
open Browser.Types

open Fable.Core
open Fable.Core.JsInterop

module Canvas =

    type CanvasRenderingContext2D with

        [<Emit("($0).roundRect($1, $2, $3, $4, $5)")>]
        member this.roundRect(x: float, y: float, w: float, h: float, radii: float) : unit =
            failwith "JS interop"

        // initialize canvas
    let canvas =
        document.getElementById "canvas"
            :?> HTMLCanvasElement

        // initialize drawing context
    let ctx = canvas.getContext_2d()

    let cellSize = 50.0

    let margin = 1.0

    let drawRegion region =
        for cell in region.Cells do
            let x = float cell.Column * cellSize
            let y = float cell.Row * cellSize
            ctx.beginPath()
            ctx.roundRect(
                x + margin,
                y + margin,
                cellSize - 2.0 * margin,
                cellSize - 2.0 * margin,
                cellSize / 10.0)
            ctx.fillStyle <- !^"blue"
            ctx.fill()

    let drawPuzzle puzzle =
        ctx.clearRect(0, 0, canvas.width, canvas.height)
        for region in puzzle.Regions do
            drawRegion region
