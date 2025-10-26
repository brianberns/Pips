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

    let drawPuzzle (puzzle : Puzzle) =

        ctx.clearRect(0, 0, canvas.width, canvas.height)

        let width = float puzzle.Board.NumCols * cellSize
        let height = float puzzle.Board.NumRows * cellSize

        ctx.beginPath()
        ctx.rect(2.0 * cellSize, 2.0 * cellSize, width, height)
        ctx.fillStyle <- !^"blue"
        ctx.fill()
