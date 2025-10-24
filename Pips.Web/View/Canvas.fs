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

    let run () =
        ctx.beginPath()
        ctx.rect(100, 200, 300, 400)
        ctx.fillStyle <- !^"blue"
        ctx.fill()
