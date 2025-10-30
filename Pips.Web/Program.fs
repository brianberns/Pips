namespace Pips.Web

open System

open Browser
open Browser.Types

open Fable.Core.JsInterop

open Thoth.Fetch

open Pips

module FetchError =

    let getMessage error =
        match error with
            | PreparingRequestFailed exn -> exn.Message
            | DecodingFailed msg -> msg
            | FetchFailed response -> response.StatusText
            | NetworkError exn -> exn.Message

module Program =

    /// Gets the element with the given ID.
    let getElement<'t when 't :> HTMLElement> id =
        document.getElementById id :?> 't

    /// Gets the current time in milliseconds.
    let getTime () =
        box (window?performance?now()) :?> float

    /// Switches to a wait cursor temporarily.
    let waitCursor () =
        document.body?style?cursor <- "wait"
        {
            new IDisposable with
                member this.Dispose() =
                    document.body?style?cursor <- "default"
        }

    /// Proxy to NY Times daily puzzle to avoid CORS restriction.
    let private dailyUrl =
        "https://pips-dsa2dqawe8hrahf7.eastus-01.azurewebsites.net/api/daily"

        // initialize elements
    let canvas : HTMLCanvasElement = getElement "puzzle-canvas"
    let puzzleDateInput : HTMLInputElement = getElement "puzzle-date"
    let solveButton : HTMLButtonElement = getElement "solve-button"
    let timerSpan : HTMLSpanElement = getElement "timer-span"
    let ctx = canvas.getContext_2d()

    /// Vertical offset of puzzle from canvas top.
    let offsetY = 10.0

    /// Translates the given canvas to center the given puzzle.
    let centerPuzzle (ctx : Context) puzzle =
        let boardWidth =
            float puzzle.Board.NumColumns * Domino.cellSize
        let offsetX = (canvas.width - boardWidth) / 2.0
        ctx.translate(offsetX, offsetY)

    /// Number of unplaced dominoes per row.
    let unplacedChunkSize = 4

    /// Translates the given canvas to center the given puzzle's
    /// unplaced dominoes.
    let centerUnplacedDominoes (ctx : Context) puzzle =
        let dominoesWidth =
            let nDominoes =
                float unplacedChunkSize
                    - (1.0 - Domino.unplacedDominoScale)
            nDominoes * (2.0 * Domino.cellSize)
        let offsetX =
            (canvas.width - dominoesWidth) / 2.0
        let offsetY =
            float (puzzle.Board.NumRows + 1) * Domino.cellSize
                + offsetY
        ctx.translate(offsetX, offsetY)

    /// Draws the given puzzle.
    let drawPuzzle (ctx : Context) puzzle =

            // draw puzzle
        ctx.resetTransform()
        centerPuzzle ctx puzzle
        Puzzle.drawPuzzle ctx puzzle

            // draw unplaced dominoes
        ctx.resetTransform()
        centerUnplacedDominoes ctx puzzle
        Puzzle.drawUnplacedDominoes
            ctx unplacedChunkSize puzzle

    /// Draws the given solutions.
    let drawSolutions (ctx : Context) puzzle solutions =
        ctx.resetTransform()
        centerPuzzle ctx puzzle
        Puzzle.drawSolutions ctx solutions

    /// Showing puzzle or solution?
    let mutable puzzleMode = true

    /// Current puzzle, if any.
    let mutable puzzleOpt = None

    /// Current solutions, if any.
    let mutable solutionsOpt = None

    /// Handles date selection event.
    let onPuzzleDateChange _ =
        promise {

                // reset
            use _ = waitCursor ()
            Canvas.cancelAnimation ()
            Canvas.clear ctx
            puzzleMode <- true
            solutionsOpt <- None
            solveButton.textContent <- "Show solution"
            solveButton.disabled <- true
            timerSpan.textContent <- ""

                // format selected date
            let dateStr =
                let date = DateTime.Parse puzzleDateInput.value
                date.ToString("yyyy-MM-dd")

                // fetch puzzle for selected date
            match! Fetch.tryGet($"{dailyUrl}?date={dateStr}") with
                | Ok daily ->

                        // convert puzzle from daily format
                    let puzzleMap = Daily.convert daily
                    let puzzle = puzzleMap["hard"]

                        // draw puzzle
                    drawPuzzle ctx puzzle

                        // save state
                    puzzleOpt <- Some puzzle
                    solveButton.disabled <- false

                | Error err ->
                    window.alert(FetchError.getMessage err)

        } |> ignore

    /// Handles solve button click event.
    let onSolveButtonClick _ =
        promise {

                // reset
            Canvas.clear ctx
            Canvas.cancelAnimation ()

                // puzzle ready to be solved?
            match puzzleMode, puzzleOpt, solutionsOpt with
                | true, Some puzzle, None ->

                        // solve puzzle
                    use _ = waitCursor ()   // doesn't work
                    let maxSolutions = 100
                    let timeStart = getTime ()
                    let solutions =
                        Backtrack.solve puzzleOpt.Value
                            |> Seq.truncate maxSolutions
                            |> Seq.toArray

                        // update timer span
                    let duration = getTime () - timeStart
                    let countStr =
                        if solutions.Length >= maxSolutions then "+"
                        else ""
                    let pluralStr =
                        if solutions.Length = 1 then "" else "s"
                    timerSpan.textContent <-
                        $"Found {solutions.Length}{countStr} solution{pluralStr} in %0.1f{duration} ms"

                        // save state
                    solutionsOpt <- Some solutions
                | _ -> ()

                // toggle mode
            puzzleMode <- not puzzleMode

                // draw solutions
            match puzzleMode, puzzleOpt, solutionsOpt with

                    // puzzle mode?
                | true, Some puzzle, _ ->
                    drawPuzzle ctx puzzle
                    solveButton.textContent <- "Show solution"

                    // solution mode?
                | false, Some puzzle, Some solutions ->
                    drawSolutions ctx puzzle solutions
                    solveButton.textContent <- "Show puzzle"

                | _ -> ()

        } |> ignore

    do
        puzzleDateInput.onchange <- onPuzzleDateChange
        solveButton.onclick <- onSolveButtonClick

        let today = DateTime.Now
        puzzleDateInput.value <- today.ToString("yyyy-MM-dd")
        Event.Create("change")
            |> puzzleDateInput.dispatchEvent
            |> ignore
