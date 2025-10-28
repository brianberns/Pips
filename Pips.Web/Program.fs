namespace Pips

open System

open Browser
open Browser.Types

open Fable.Core.JsInterop

open Thoth.Fetch

type WaitCursor() =

    do document.body?style?cursor <- "wait"

    interface IDisposable with
        member this.Dispose() =
            document.body?style?cursor <- "default"

module FetchError =

    let getMessage error =
        match error with
            | PreparingRequestFailed exn -> exn.Message
            | DecodingFailed msg -> msg
            | FetchFailed response -> response.StatusText
            | NetworkError exn -> exn.Message

module Program =

    let getCanvas id =
        document.getElementById id
            :?> HTMLCanvasElement

    let clearCanvas (ctx: CanvasRenderingContext2D) =
        ctx.clearRect(0.0, 0.0, ctx.canvas.width, ctx.canvas.height)

        // initialize canvases
    let puzzleCanvas = getCanvas "puzzle-canvas"
    let solutionCanvas = getCanvas "solution-canvas"

        // initialize drawing contexts
    let puzzleCtx = puzzleCanvas.getContext_2d()
    let solutionCtx = solutionCanvas.getContext_2d()

    let private dailyUrl =
        "https://pips-dsa2dqawe8hrahf7.eastus-01.azurewebsites.net/api/daily"

    let mutable puzzleOpt = None

    let puzzleDateInput =
        document.getElementById "puzzle-date"
            :?> HTMLInputElement
    puzzleDateInput.onchange <- (fun _ ->
        promise {
            use _ = new WaitCursor()

            clearCanvas puzzleCtx
            clearCanvas solutionCtx

            let date =
                puzzleDateInput.value
                    |> DateTime.Parse
            let dateStr = date.ToString("yyyy-MM-dd")
            match! Fetch.tryGet($"{dailyUrl}?date={dateStr}") with
                | Ok daily ->
                    let puzzleMap = Daily.convert daily
                    let puzzle = puzzleMap["hard"]
                    Canvas.drawPuzzle puzzleCtx puzzle
                    puzzleOpt <- Some puzzle
                | Error err ->
                    window.alert(FetchError.getMessage err)
        } |> ignore)

    let solveButton =
        document.getElementById "solve-button"
            :?> HTMLButtonElement
    solveButton.onclick <- (fun _ ->
        promise {
            use _ = new WaitCursor()
            Backtrack.solve puzzleOpt.Value
                |> Seq.truncate 10
                |> Seq.toArray
                |> Canvas.drawSolutions solutionCtx
        } |> ignore)
