namespace Pips.Web

open System

open Browser
open Browser.Types

open Fable.Core.JsInterop

open Thoth.Fetch

open Pips

module FetchError =

    /// Extracts error message.
    let getMessage = function
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
    let prevDateButton : HTMLButtonElement = getElement "previous-date-button"
    let nextDateButton : HTMLButtonElement = getElement "next-date-button"
    let difficultySelect : HTMLSelectElement = getElement "difficulty-select"
    let solveButton : HTMLButtonElement = getElement "solve-button"
    let pauseButton : HTMLButtonElement = getElement "pause-button"
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
    let drawSolutions (ctx : Context) puzzle animate solutions =
        ctx.resetTransform()
        centerPuzzle ctx puzzle
        Puzzle.drawSolutions ctx animate solutions

    /// Showing puzzle or solution?
    let mutable puzzleMode = true

    /// Current puzzle, if any.
    let mutable puzzleOpt = None

    /// Current solutions, if any.
    let mutable solutionsOpt = None

    /// Animation has been paused?
    let mutable animationPaused = false

        // display strings
    let showSolutionStr = "Show solution"
    let showPuzzleStr = "Show puzzle"
    let pauseStr = "⏯️"
    let playStr = "⏯️"

    /// Handles date or difficulty selection event.
    let onPuzzleChange _ =
        promise {

                // reset
            use _ = waitCursor ()
            Canvas.cancelAnimation ()
            Canvas.clear ctx
            puzzleMode <- true
            solutionsOpt <- None
            animationPaused <- false
            solveButton.textContent <- showSolutionStr
            solveButton.disabled <- true
            pauseButton.textContent <- pauseStr
            pauseButton.disabled <- true
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
                    let puzzle = puzzleMap[difficultySelect.value]

                        // draw puzzle
                    drawPuzzle ctx puzzle

                        // save state
                    puzzleOpt <- Some puzzle
                    solveButton.disabled <- false

                | Error err ->
                    window.alert(FetchError.getMessage err)

        } |> ignore

    /// Minimum date.
    let minDate = DateTime.Parse puzzleDateInput.min

    /// Triggers a date change event.
    let triggerDateChangeEvent () =
        Event.Create("change")
            |> puzzleDateInput.dispatchEvent
            |> ignore

    /// Increments the puzzle date.
    let incrDate incr =
        let date = DateTime.Parse puzzleDateInput.value
        let date = date.AddDays(incr)
        if date >= minDate then
            puzzleDateInput.value <- date.ToString("yyyy-MM-dd")
            triggerDateChangeEvent ()

    /// Handles previous date button click event.
    let onPrevDateButtonClick _ = incrDate -1.0

    /// Handles next date button click event.
    let onNextDateButtonClick _ = incrDate 1.0

    /// Handles solve button click event.
    let onSolveButtonClick _ =
        promise {

                // reset
            Canvas.cancelAnimation ()
            Canvas.clear ctx

                // puzzle ready to be solved?
            match puzzleMode, puzzleOpt, solutionsOpt with
                | true, Some puzzle, None ->

                        // solve puzzle
                    use _ = waitCursor ()   // doesn't work
                    let maxSolutions = 1000
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

                // draw puzzle or solutions
            match puzzleMode, puzzleOpt, solutionsOpt with

                    // puzzle mode
                | true, Some puzzle, _ ->
                    drawPuzzle ctx puzzle
                    solveButton.textContent <- showSolutionStr
                    pauseButton.disabled <- true

                    // solution mode
                | false, Some puzzle, Some solutions ->
                    let animate =
                        not animationPaused && solutions.Length > 1
                    drawSolutions ctx puzzle animate solutions
                    solveButton.textContent <- showPuzzleStr
                    if solutions.Length > 1 then
                        pauseButton.disabled <- false

                | _ -> ()

        } |> ignore

    /// Handles pause button click event.
    let onPauseButtonClick _ =
        match animationPaused, puzzleOpt, solutionsOpt with

                // pause animation?
            | false, _, _ ->
                Canvas.cancelAnimation ()
                pauseButton.textContent <- playStr

                // restart animation?
            | true, Some puzzle, Some solutions ->
                Canvas.clear ctx
                drawSolutions ctx puzzle true solutions
                pauseButton.textContent <- pauseStr

            | _ -> ()

            // toggle state
        animationPaused <- not animationPaused

    do
            // setup event handlers
        prevDateButton.onclick <- onPrevDateButtonClick
        nextDateButton.onclick <- onNextDateButtonClick
        puzzleDateInput.onchange <- onPuzzleChange
        difficultySelect.onchange <- onPuzzleChange
        solveButton.onclick <- onSolveButtonClick
        pauseButton.onclick <- onPauseButtonClick

            // start with today's puzzle
        let today = DateTime.Now
        puzzleDateInput.value <- today.ToString("yyyy-MM-dd")
        triggerDateChangeEvent ()
