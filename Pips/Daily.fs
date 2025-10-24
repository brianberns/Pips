namespace Pips

(*
 * Support for NY Times daily JSON format.
 *)

open System
open System.IO
open System.Net.Http
open System.Text.Json

type DailyRegion =
    {
        indices : int[][]
        ``type`` : string
        target : int
    }

module DailyRegion =

    let convert region =
        let cells =
            region.indices
                |> Array.map (fun pair ->
                    assert(pair.Length = 2)
                    Cell.create pair[0] pair[1])
        let typ =
            match region.``type`` with
                | "empty"   -> RegionType.Any
                | "equals"  -> RegionType.Equal
                | "greater" -> RegionType.SumGreater region.target
                | "less"    -> RegionType.SumLess region.target
                | "sum"     -> RegionType.SumExact region.target
                | "unequal" -> RegionType.Unequal
                | typ -> failwith $"Unexpected region type: {typ}"
        {
            Cells = cells
            Type = typ
        }

type DailyPuzzle =
    {
        id : int
        constructors : string
        dominoes : int[][]
        regions : DailyRegion[]
        solution : int[][][]
    }

module DailyPuzzle =

    let convert puzzle =
        let dominoes =
            puzzle.dominoes
                |> Array.map (fun pair ->
                    assert(pair.Length = 2)
                    Domino.create pair[0] pair[1])
        let regions =
            puzzle.regions
                |> Array.map DailyRegion.convert
        Puzzle.create dominoes regions

type Daily =
    {
        printDate : DateOnly
        editor : string
        easy : DailyPuzzle
        medium : DailyPuzzle
        hard : DailyPuzzle
    }

module Daily =

    /// Deserializes puzzles from the given JSON text.
    let private deserialize (text : string) =
        let daily = JsonSerializer.Deserialize<Daily>(text)
        Map [
            "easy", daily.easy
            "medium", daily.medium
            "hard", daily.hard
        ] |> Map.map (fun _ puzzle -> DailyPuzzle.convert puzzle)

    /// Loads puzzles from the given JSON file.
    let loadFile =
        File.ReadAllText >> deserialize

    /// Loads puzzles from the given JSON URL.
    /// E.g. "https://www.nytimes.com/svc/pips/v1/2025-10-14.json".
    let loadHttpAsync (uri : string) =
        use client = new HttpClient()
        task {
            let! text = client.GetStringAsync(uri)
            return deserialize text
        }

    /// Loads puzzles from the given JSON URL.
    /// E.g. "https://www.nytimes.com/svc/pips/v1/2025-10-14.json".
    let loadHttp uri =
        loadHttpAsync uri
            |> Async.AwaitTask
            |> Async.RunSynchronously
