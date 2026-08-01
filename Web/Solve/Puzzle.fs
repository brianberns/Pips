namespace Pips.Web

open Feliz
open Pips

module Puzzle =

    /// Maps each of the given puzzle's cells to its region.
    let private getRegionMap puzzle =
        Map [
            for region in puzzle.Regions do
                for cell in region.Cells do
                    yield cell, region
        ]

    /// Renders the given puzzle's board. If a solution is given,
    /// its dominoes are laid over the board instead of the
    /// board's regions.
    let renderBoard puzzle solutionOpt =
        let regionMap = getRegionMap puzzle
        Html.div [
            prop.classes [
                "board"
                if Option.isSome solutionOpt then "board-solved"
            ]
            prop.style [
                style.custom ("--rows", $"{puzzle.Board.NumRows}")
                style.custom ("--cols", $"{puzzle.Board.NumColumns}")
            ]
            prop.children [

                    // regions of the board
                for region in puzzle.Regions do
                    yield! Region.render regionMap region

                    // dominoes of the solution, if any
                match solutionOpt with
                    | Some (solution : Puzzle) ->
                        for (domino, edge) in solution.Board.DominoPlaces do
                            Domino.renderPlaced domino edge
                    | None -> ()
            ]
        ]

    /// Renders the given puzzle's unplaced dominoes.
    let renderUnplacedDominoes puzzle =
        Html.div [
            prop.className "tray"
            prop.children [
                for domino in puzzle.UnplacedDominoes do
                    Domino.renderUnplaced domino
            ]
        ]
