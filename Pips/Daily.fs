namespace Pips

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
                | "empty"   -> RegionType.Unconstrained
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
                    { Left = pair[0]; Right = pair[1] })
        let regions =
            puzzle.regions
                |> Array.map DailyRegion.convert
        Puzzle.create dominoes regions

module Daily =

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

    let loadFile =
        File.ReadAllText >> parse

    let loadHttp (uri : string) =
        use client = new HttpClient()
        client.GetStringAsync(uri)
            .Result
            |> parse
