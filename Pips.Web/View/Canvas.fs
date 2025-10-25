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
                let! (daily : Daily) =
                    Fetch.get("https://pips-dsa2dqawe8hrahf7.eastus-01.azurewebsites.net/api/daily?date=10-25-2025")
                let puzzleMap = Daily.convert daily
                console.log(puzzleMap["easy"])
            }
        Promise.start work

        ctx.beginPath()
        ctx.rect(100, 200, 300, 400)
        ctx.fillStyle <- !^"blue"
        ctx.fill()
