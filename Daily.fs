namespace Pips

open System
open System.Net.Http
open System.Text.Json

[<CLIMutable>]
type JsonRegion =
    {
        indices : int[][]
        ``type`` : string
        target : int
    }

[<CLIMutable>]
type JsonPuzzle =
    {
        id : int
        constructors : string
        dominoes : int[][]
        regions : JsonRegion[]
        solution : int[][][]
    }

module Daily =

    (*
    let private fromJson (jsonPuzzle : JsonPuzzle) =
        let dominoes =
            jsonPuzzle.dominoes
                |> Array.map (fun d -> { Left = d[0]; Right = d[1] })
                |> Array.toList
        let regions =
            jsonPuzzle.regions
                |> Array.map Region.fromJson
        let board = Board.empty
        Puzzle.create dominoes regions
    *)

    let parse (uri : string) =
        use client = new HttpClient()
        let content = client.GetStringAsync(uri).Result
        let options = JsonSerializerOptions()
        let dailyJson = JsonDocument.Parse(content)
        let root = dailyJson.RootElement
        Map [
            for prop in root.EnumerateObject() do
                match prop.Name with
                    | "printDate"
                    | "editor" -> ()
                    | _ ->
                        let puzzle =
                            JsonSerializer.Deserialize<JsonPuzzle>(
                                prop.Value.GetRawText(), options)
                        yield prop.Name, puzzle
        ]
