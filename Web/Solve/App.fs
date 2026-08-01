namespace Pips.Web

open System
open Browser
open Elmish
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

/// Difficulty of a daily puzzle.
type Difficulty =
    | Easy
    | Medium
    | Hard

module Difficulty =

    /// All difficulties, from easiest to hardest.
    let all = [ Easy; Medium; Hard ]

    /// Key of the given difficulty in a daily puzzle map.
    let toKey = function
        | Easy -> "easy"
        | Medium -> "medium"
        | Hard -> "hard"

    /// Display string.
    let toString = function
        | Easy -> "Easy"
        | Medium -> "Medium"
        | Hard -> "Hard"

    /// Difficulty of the given key, if any.
    let tryOfKey key =
        all
            |> List.tryFind (fun difficulty ->
                toKey difficulty = key)

/// Solutions found for a puzzle.
type Solutions =
    {
        /// Solved puzzles.
        Puzzles : Puzzle[]

        /// Were there more solutions than we looked for?
        Truncated : bool

        /// Time taken to find the solutions, in milliseconds.
        Duration : float
    }

/// Is the board showing a puzzle or its solutions?
type BoardView =
    | PuzzleView
    | SolutionView

/// State of the application.
type Model =
    {
        /// Date of the daily puzzle.
        Date : DateTime

        /// Selected difficulty.
        Difficulty : Difficulty

        /// Puzzles fetched for the selected date, by difficulty
        /// key.
        Puzzles : Map<string, Puzzle>

        /// Fetching puzzles?
        Loading : bool

        /// Most recent error, if any.
        Error : string option

        /// Solutions to the selected puzzle, if it has been
        /// solved.
        Solutions : Solutions option

        /// Solving the selected puzzle?
        Solving : bool

        /// Showing puzzle or solutions?
        View : BoardView

        /// Solution animation has been paused?
        Paused : bool

        /// Index of the solution being displayed.
        Frame : int
    }

/// Event that changes the state of the application.
type Msg =
    | SetDate of DateTime
    | OffsetDate of float
    | SetDifficulty of Difficulty
    | PuzzlesLoaded of Result<Map<string, Puzzle>, string>
    | ToggleView
    | SolutionsFound of Solutions
    | TogglePause
    | NextFrame

module Model =

    /// Puzzle currently selected, if any.
    let tryGetPuzzle model =
        Map.tryFind
            (Difficulty.toKey model.Difficulty)
            model.Puzzles

    /// Number of solutions found, if any.
    let getNumSolutions model =
        model.Solutions
            |> Option.map (fun solutions ->
                solutions.Puzzles.Length)
            |> Option.defaultValue 0

    /// Solution currently displayed, if any.
    let tryGetSolution model =
        match model.View, model.Solutions with
            | SolutionView, Some solutions
                when solutions.Puzzles.Length > 0 ->
                let iSolution =
                    model.Frame % solutions.Puzzles.Length
                Some solutions.Puzzles[iSolution]
            | _ -> None

    /// Is the solution animation running?
    let isAnimating model =
        model.View = SolutionView
            && not model.Paused
            && getNumSolutions model > 1

module App =

    /// Earliest puzzle available.
    let minDate = DateTime(2025, 8, 18)

    /// Maximum number of solutions to look for.
    let private maxSolutions = 1000

    /// Solution animation rate.
    let private framesPerSecond = 10.0

    /// Proxy to NY Times daily puzzle to avoid CORS restriction.
    let private dailyUrl =
        "https://pips-dsa2dqawe8hrahf7.eastus-01.azurewebsites.net/api/daily"

    /// Fetches the daily puzzles for the given date.
    let private loadPuzzles (date : DateTime) =
        let dateStr = date.ToString("yyyy-MM-dd")
        Cmd.OfPromise.either
            (fun () ->
                Fetch.tryGet<_, Daily>($"{dailyUrl}?date={dateStr}"))
            ()
            (fun result ->
                result
                    |> Result.map Daily.convert
                    |> Result.mapError FetchError.getMessage
                    |> PuzzlesLoaded)
            (fun exn ->
                PuzzlesLoaded (Error exn.Message))

    /// Gets the current time in milliseconds.
    let private getTime () =
        box (window?performance?now()) :?> float

    /// Solves the given puzzle. The search runs on the UI thread,
    /// so we yield first to let the browser paint.
    let private solvePuzzle puzzle =
        Cmd.OfAsync.perform
            (fun () ->
                async {
                    do! Async.Sleep 16
                    let timeStart = getTime ()
                    let puzzles =
                        Backtrack.solve puzzle
                            |> Seq.truncate maxSolutions
                            |> Seq.toArray
                    return {
                        Puzzles = puzzles
                        Truncated =
                            puzzles.Length >= maxSolutions
                        Duration = getTime () - timeStart
                    }
                })
            ()
            SolutionsFound

    /// Discards any solutions found for the previous puzzle.
    let private resetSolutions model =
        {
            model with
                Solutions = None
                Solving = false
                View = PuzzleView
                Paused = false
                Frame = 0
        }

    /// Initializes the application with today's puzzle.
    let init () =
        let today = DateTime.Now.Date
        let model =
            {
                Date = today
                Difficulty = Hard
                Puzzles = Map.empty
                Loading = true
                Error = None
                Solutions = None
                Solving = false
                View = PuzzleView
                Paused = false
                Frame = 0
            }
        model, loadPuzzles today

    /// Updates the application state in response to an event.
    let update msg model =
        match msg with

                // fetch the puzzles for a new date
            | SetDate date ->
                let model =
                    {
                        resetSolutions model with
                            Date = date
                            Puzzles = Map.empty
                            Loading = true
                            Error = None
                    }
                model, loadPuzzles date

                // step to an adjacent date
            | OffsetDate days ->
                let date = model.Date.AddDays(days)
                if date >= minDate then
                    model, Cmd.ofMsg (SetDate date)
                else model, Cmd.none

                // the puzzles for this date are already loaded
            | SetDifficulty difficulty ->
                let model =
                    {
                        resetSolutions model with
                            Difficulty = difficulty
                    }
                model, Cmd.none

            | PuzzlesLoaded (Ok puzzles) ->
                let model =
                    {
                        model with
                            Puzzles = puzzles
                            Loading = false
                    }
                model, Cmd.none

            | PuzzlesLoaded (Error message) ->
                let model =
                    {
                        model with
                            Loading = false
                            Error = Some message
                    }
                model, Cmd.none

                // toggle between puzzle and solutions, solving
                // the puzzle first if necessary
            | ToggleView ->
                match model.View,
                      model.Solutions,
                      Model.tryGetPuzzle model with
                    | PuzzleView, None, Some puzzle ->
                        { model with Solving = true },
                        solvePuzzle puzzle
                    | PuzzleView, Some _, _ ->
                        { model with View = SolutionView },
                        Cmd.none
                    | SolutionView, _, _ ->
                        { model with View = PuzzleView },
                        Cmd.none
                    | _ -> model, Cmd.none

            | SolutionsFound solutions ->
                let model =
                    {
                        model with
                            Solutions = Some solutions
                            Solving = false
                            Frame = 0
                            View =
                                if solutions.Puzzles.Length > 0 then
                                    SolutionView
                                else PuzzleView
                    }
                model, Cmd.none

            | TogglePause ->
                { model with Paused = not model.Paused },
                Cmd.none

            | NextFrame ->
                { model with Frame = model.Frame + 1 },
                Cmd.none

    /// Animates the solutions by advancing a frame at a time.
    let private animate dispatch =
        let intervalId =
            window.setInterval(
                (fun () -> dispatch NextFrame),
                int (1000.0 / framesPerSecond))
        {
            new IDisposable with
                member _.Dispose() =
                    window.clearInterval intervalId
        }

    /// Subscribes to the animation timer when needed.
    let subscribe model =
        [
            if Model.isAnimating model then
                [ "animation" ], animate
        ]
