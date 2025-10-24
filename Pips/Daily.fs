namespace Pips

open System
open System.IO

#if FABLE_COMPILER
open Thoth.Fetch
#else
open System.Net.Http
open System.Text.Json
#endif

(*
 * Support for NY Times daily JSON format.
 *)

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

    /// Converts from NY Times daily structure to internal representation.
    let private convert daily =
        Map [
            "easy", DailyPuzzle.convert daily.easy
            "medium", DailyPuzzle.convert daily.medium
            "hard", DailyPuzzle.convert daily.hard
        ]

#if !FABLE_COMPILER

    /// Parses puzzles from the given JSON text.
    let parse (text : string) =
        JsonSerializer.Deserialize<Daily>(text)
            |> convert

    /// Loads puzzles from the given JSON file.
    let loadFile =
        File.ReadAllText >> parse
#endif

    /// Loads puzzles from the given JSON URL.
    /// E.g. "https://www.nytimes.com/svc/pips/v1/2025-10-14.json".
#if FABLE_COMPILER
    let loadHttp (uri : string) =
        promise {
            let! result = Fetch.get(uri)
            return convert result
        }
#else
    let loadHttp (uri : string) =
        use client = new HttpClient()
        task {
            let! text = client.GetStringAsync(uri)
            return parse text
        }
#endif
