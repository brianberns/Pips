namespace Pips

(*
 * Support for NY Times daily JSON format.
 *)

open System
open System.IO
#if !FABLE_COMPILER
open System.Net.Http
open System.Text.Json
#endif

type DailyRegion =
    {
        indices : int[][]
        ``type`` : string
        target : Option<int>
    }

module DailyRegion =

    let convert region =
        let cells =
            region.indices
                |> Array.map (fun pair ->
                    assert(pair.Length = 2)
                    Cell.create pair[0] pair[1])
        let typ =
            match region.``type``, region.target with
                | "empty", None          -> RegionType.Any
                | "equals", None         -> RegionType.Equal
                | "greater", Some target -> RegionType.SumGreater target
                | "less", Some target    -> RegionType.SumLess target
                | "sum", Some target     -> RegionType.SumExact target
                | "unequal", None        -> RegionType.Unequal
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

    let tryConvert puzzle =
        if isNull puzzle.dominoes then None   // NY Times deletes puzzles sometimes?
        else
            let dominoes =
                puzzle.dominoes
                    |> Array.map (fun pair ->
                        assert(pair.Length = 2)
                        Domino.create pair[0] pair[1])
            let regions =
                puzzle.regions
                    |> Array.map DailyRegion.convert
            Some (Puzzle.create dominoes regions)

type Daily =
    {
        easy : DailyPuzzle
        medium : DailyPuzzle
        hard : DailyPuzzle
    }

module Daily =

    /// Converts a daily to a map of puzzles.
    let convert daily =
        [
            "easy", daily.easy
            "medium", daily.medium
            "hard", daily.hard
        ]
            |> Seq.choose (fun (name, puzzle) ->
                DailyPuzzle.tryConvert puzzle
                    |> Option.map (fun puzzle ->
                        name, puzzle))
            |> Map

#if !FABLE_COMPILER
    /// Deserializes puzzles from the given JSON text.
    let private deserialize (text : string) =
        JsonSerializer.Deserialize<Daily>(text)
            |> convert

    /// Loads puzzles from the given JSON file.
    let loadFile =
        File.ReadAllText >> deserialize

    /// Loads puzzles from the given JSON URL.
    /// E.g. "https://www.nytimes.com/svc/pips/v1/2025-10-14.json".
    let loadHttpAsync (uri : string) =
        task {
            use client = new HttpClient()
            let! text = client.GetStringAsync(uri)
            return deserialize text
        }

    /// Loads puzzles from the given JSON URL.
    /// E.g. "https://www.nytimes.com/svc/pips/v1/2025-10-14.json".
    let loadHttp uri =
        loadHttpAsync uri
            |> Async.AwaitTask
            |> Async.RunSynchronously
#endif
