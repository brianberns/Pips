namespace Pips

open Browser
open Browser.Types

open Fable.Core.JsInterop

open Thoth.Fetch

module Canvas =

        // initialize canvas
    let canvas =
        document.getElementById "canvas"
            :?> HTMLCanvasElement

        // initialize drawing context
    let ctx = canvas.getContext_2d()

    let run () =

        let work =
            promise {
                let! puzzleMap =
                    Fetch.get<_, Map<string, Puzzle>>("http://localhost:7071/api/Daily?date=10-25-2025")
                console.log puzzleMap["hard"]
            }
        Promise.start work

        ctx.beginPath()
        ctx.rect(100, 200, 300, 400)
        ctx.fillStyle <- !^"blue"
        ctx.fill()
