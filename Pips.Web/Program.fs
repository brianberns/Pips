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

    let private dailyUrl =
        "https://pips-dsa2dqawe8hrahf7.eastus-01.azurewebsites.net/api/daily"

    let puzzleDateInput =
        document.getElementById "puzzle-date"
            :?> HTMLInputElement
    puzzleDateInput.onchange <- (fun _ ->
        promise {
            use _ = new WaitCursor()
            let date =
                puzzleDateInput.value
                    |> DateTime.Parse
            let dateStr = date.ToString("yyyy-MM-dd")
            match! Fetch.tryGet($"{dailyUrl}?date={dateStr}") with
                | Ok daily ->
                    let puzzleMap = Daily.convert daily
                    Canvas.drawPuzzle(puzzleMap["easy"])
                | Error err ->
                    window.alert(FetchError.getMessage err)
        } |> ignore)
