
namespace Pips

open BenchmarkDotNet.Attributes

module Puzzle =

    let puzzleMap =
        Daily.loadHttp "https://www.nytimes.com/svc/pips/v1/2025-10-14.json"

    let solveOne () =
        Puzzle.solve puzzleMap["hard"]
            |> ignore

[<MemoryDiagnoser>]
type Benchmarks() =

    [<Benchmark>]
    member _.SolveOne() =
        Puzzle.solveOne()
