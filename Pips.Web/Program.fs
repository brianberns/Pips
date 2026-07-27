namespace Pips.Web

open System

open Browser

open Elmish

open Feliz
open Feliz.UseElmish

module View =

    /// Formats the given date the way an HTML date input
    /// expects it.
    let private toDateString (date : DateTime) =
        date.ToString("yyyy-MM-dd")

    /// Renders the puzzle date controls.
    let private renderDateControls model dispatch =
        Html.div [
            prop.className "control"
            prop.children [
                Html.label [
                    prop.htmlFor "puzzle-date"
                    prop.text "Puzzle date"
                ]
                Html.div [
                    prop.className "control-row"
                    prop.children [
                        Html.button [
                            prop.className "step-button"
                            prop.type' "button"
                            prop.ariaLabel "Previous puzzle"
                            prop.disabled (
                                model.Date <= App.minDate)
                            prop.onClick (fun _ ->
                                dispatch (OffsetDate -1.0))
                            prop.text "◀"
                        ]
                        Html.input [
                            prop.id "puzzle-date"
                            prop.type' "date"
                            prop.value (toDateString model.Date)
                            prop.custom (
                                "min", toDateString App.minDate)
                            prop.onChange (fun (value : string) ->
                                match DateTime.TryParse value with
                                    | true, date ->
                                        dispatch (SetDate date)
                                    | _ -> ())
                        ]
                        Html.button [
                            prop.className "step-button"
                            prop.type' "button"
                            prop.ariaLabel "Next puzzle"
                            prop.onClick (fun _ ->
                                dispatch (OffsetDate 1.0))
                            prop.text "▶"
                        ]
                    ]
                ]
            ]
        ]

    /// Renders the difficulty control.
    let private renderDifficultyControl model dispatch =
        Html.div [
            prop.className "control"
            prop.children [
                Html.label [
                    prop.htmlFor "difficulty"
                    prop.text "Difficulty"
                ]
                Html.select [
                    prop.id "difficulty"
                    prop.value (
                        Difficulty.toKey model.Difficulty)
                    prop.onChange (fun (value : string) ->
                        Difficulty.tryOfKey value
                            |> Option.iter (
                                SetDifficulty >> dispatch))
                    prop.children [
                        for difficulty in Difficulty.all do
                            Html.option [
                                prop.key (
                                    Difficulty.toKey difficulty)
                                prop.value (
                                    Difficulty.toKey difficulty)
                                prop.text (
                                    Difficulty.toString difficulty)
                            ]
                    ]
                ]
            ]
        ]

    /// Renders the solve and pause buttons.
    let private renderActions model dispatch =

        let solveText =
            match model.View with
                | PuzzleView ->
                    if model.Solving then "Solving…"
                    else "Show solution"
                | SolutionView -> "Show puzzle"

        Html.div [
            prop.className "control-row actions"
            prop.children [
                Html.button [
                    prop.className "primary-button"
                    prop.type' "button"
                    prop.disabled (
                        model.Solving
                            || (Model.tryGetPuzzle model)
                                .IsNone)
                    prop.onClick (fun _ -> dispatch ToggleView)
                    prop.text solveText
                ]
                Html.button [
                    prop.className "step-button"
                    prop.type' "button"
                    prop.ariaLabel (
                        if model.Paused then "Resume"
                        else "Pause")
                    prop.disabled (
                        model.View <> SolutionView
                            || Model.getNumSolutions model <= 1)
                    prop.onClick (fun _ -> dispatch TogglePause)
                    prop.text (
                        if model.Paused then "▶️" else "⏸️")
                ]
            ]
        ]

    /// Renders the status line.
    let private renderStatus model =

        let summarize solutions =
            let countStr =
                if solutions.Truncated then "+" else ""
            let pluralStr =
                if solutions.Puzzles.Length = 1 then ""
                else "s"
            $"Found {solutions.Puzzles.Length}{countStr} \
                solution{pluralStr} in \
                %0.1f{solutions.Duration} ms"

        match model.Error, model.Loading, model.Solutions with
            | Some message, _, _ ->
                Html.p [
                    prop.className "status status-error"
                    prop.text message
                ]
            | None, true, _ ->
                Html.p [
                    prop.className "status"
                    prop.text "Loading puzzle…"
                ]
            | None, false, Some solutions ->
                Html.p [
                    prop.className "status"
                    prop.text (summarize solutions)
                ]
            | None, false, None ->
                Html.p [
                    prop.className "status"
                    prop.text ""
                ]

    /// Renders the board and any dominoes not yet placed on it.
    let private renderBoard model =
        match Model.tryGetPuzzle model with
            | Some puzzle ->
                let solutionOpt = Model.tryGetSolution model
                Html.div [
                    prop.className "board-area"
                    prop.children [
                        Puzzle.renderBoard puzzle solutionOpt
                        if solutionOpt.IsNone then
                            Puzzle.renderUnplacedDominoes puzzle
                    ]
                ]
            | None ->
                Html.div [ prop.className "board-area" ]

    /// Renders the application.
    [<ReactComponent>]
    let App () =

        let model, dispatch =
            React.useElmish(fun () ->
                Program.mkProgram App.init App.update
                    (fun _ _ -> ())
                    |> Program.withSubscription App.subscribe)

        Html.div [
            prop.classes [
                "app"
                if model.Solving then "solving"
            ]
            prop.children [
                Html.h1 "Pips solver"
                Html.div [
                    prop.className "controls"
                    prop.children [
                        renderDateControls model dispatch
                        renderDifficultyControl model dispatch
                    ]
                ]
                renderActions model dispatch
                renderStatus model
                renderBoard model
                Html.footer [
                    Html.text "Pips is a game from the "
                    Html.a [
                        prop.href
                            "https://www.nytimes.com/games/pips"
                        prop.text "New York Times"
                    ]
                ]
            ]
        ]

module Main =

    /// Mounts the application.
    let private root =
        ReactDOM.createRoot(
            document.getElementById "root")

    root.render(View.App())
