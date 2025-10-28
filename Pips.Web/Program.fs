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

    let getTime () =
        box (window?performance?now()) :?> float

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

    let solveButton =
        document.getElementById "solve-button"
            :?> HTMLButtonElement

    let timerLabel =
        document.getElementById "timer-label"
            :?> HTMLLabelElement

    puzzleDateInput.onchange <- (fun _ ->
        promise {

                // reset
            use _ = new WaitCursor()
            Canvas.clear puzzleCtx
            Canvas.clear solutionCtx
            Canvas.cancelAnimation ()
            solveButton.disabled <- true
            timerLabel.textContent <- ""

                // fetch puzzle for selected date
            let date =
                puzzleDateInput.value
                    |> DateTime.Parse
            let dateStr = date.ToString("yyyy-MM-dd")
            match! Fetch.tryGet($"{dailyUrl}?date={dateStr}") with
                | Ok daily ->

                        // convert and draw puzzle
                    let puzzleMap = Daily.convert daily
                    let puzzle = puzzleMap["hard"]
                    Canvas.drawPuzzle puzzleCtx puzzle

                        // save state
                    puzzleOpt <- Some puzzle
                    solveButton.disabled <- false

                | Error err ->
                    window.alert(FetchError.getMessage err)
        } |> ignore)

    solveButton.onclick <- (fun _ ->
        promise {
            match puzzleOpt with
                | Some puzzle ->

                        // reset
                    use _ = new WaitCursor()   // doesn't work
                    Canvas.clear solutionCtx
                    Canvas.cancelAnimation ()

                        // solve puzzle
                    let maxSolutions = 100
                    let timeStart = getTime ()
                    let solutions =
                        Backtrack.solve puzzleOpt.Value
                            |> Seq.truncate maxSolutions
                            |> Seq.toArray

                        // update timer label
                    let duration = getTime () - timeStart
                    let countStr =
                        if solutions.Length >= maxSolutions then "+"
                        else ""
                    let pluralStr =
                        if solutions.Length = 1 then "" else "s"
                    timerLabel.textContent <-
                        $"Found {solutions.Length}{countStr} solution{pluralStr} in %0.1f{duration} ms"

                        // draw solutions
                    Canvas.drawSolutions solutionCtx solutions

                | None -> ()
        } |> ignore)

    let today = DateTime.Now
    puzzleDateInput.value <- today.ToString("yyyy-MM-dd")
    Event.Create("change")
        |> puzzleDateInput.dispatchEvent
        |> ignore
