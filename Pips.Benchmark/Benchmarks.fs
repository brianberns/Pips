
namespace Pips

open BenchmarkDotNet.Attributes

module Puzzle =

    let puzzle =
        (Daily.loadHttp "https://www.nytimes.com/svc/pips/v1/2025-10-14.json")
            |> Map.find "hard"

    let solveOne () =
        Backtrack.solve puzzle
            |> ignore

[<MemoryDiagnoser>]
type Benchmarks() =

    [<Benchmark>]
    member _.SolveOne() =
        Puzzle.solveOne()
