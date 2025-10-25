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

    let private dailyUrl =
        "https://pips-dsa2dqawe8hrahf7.eastus-01.azurewebsites.net/api/daily"

    let run () =

        let work =
            promise {
                let dateStr = System.DateTime.Today.ToString("yyyy-MM-dd")
                let! (daily : Daily) =
                    Fetch.get($"{dailyUrl}?date={dateStr}")
                let puzzleMap = Daily.convert daily
                console.log(puzzleMap["easy"])
            }
        Promise.start work

        ctx.beginPath()
        ctx.rect(100, 200, 300, 400)
        ctx.fillStyle <- !^"blue"
        ctx.fill()
