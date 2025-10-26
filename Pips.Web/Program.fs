namespace Pips

open System

open Browser
open Browser.Types

open Thoth.Fetch

module Program =

    let private dailyUrl =
        "https://pips-dsa2dqawe8hrahf7.eastus-01.azurewebsites.net/api/daily"

    let puzzleDateInput =
        document.getElementById "puzzle-date"
            :?> HTMLInputElement
    puzzleDateInput.onchange <- (fun _ ->
        promise {
            let date =
                puzzleDateInput.value
                    |> DateTime.Parse
            let dateStr = date.ToString("yyyy-MM-dd")
            let! (daily : Daily) =
                Fetch.get($"{dailyUrl}?date={dateStr}")
            let puzzleMap = Daily.convert daily
            Canvas.drawPuzzle(puzzleMap["easy"])
        } |> ignore)
