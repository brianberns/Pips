namespace Pips

open System.IO
open System.Net.Http
open System.Text.Json

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
                | "sum"     -> RegionType.Sum region.target
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

module Daily =

    /// Parses puzzles from the given JSON text.
    let parse (text : string) =
        let dailyJson = JsonDocument.Parse(text)
        let root = dailyJson.RootElement
        Map [
            for prop in root.EnumerateObject() do
                match prop.Name with
                    | "printDate"
                    | "editor" -> ()
                    | _ ->
                        let puzzle =
                            JsonSerializer.Deserialize<DailyPuzzle>(
                                prop.Value.GetRawText())
                        yield prop.Name, DailyPuzzle.convert puzzle
        ]

    /// Loads puzzles from the given JSON file.
    let loadFile =
        File.ReadAllText >> parse

    /// Loads puzzles from the given JSON URL.
    /// E.g. "https://www.nytimes.com/svc/pips/v1/2025-10-14.json".
    let loadHttp (uri : string) =
        use client = new HttpClient()
        client.GetStringAsync(uri)
            .Result
            |> parse
