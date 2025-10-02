namespace Pips

open System.Diagnostics

module Program =

    let puzzleMap = Daily.loadHttp "https://www.nytimes.com/svc/pips/v1/2025-09-25.json"

    let stopwatch = Stopwatch.StartNew()
    let solutions = Puzzle.solve puzzleMap["hard"]
    printfn $"Found {solutions.Length} solution(s) in {stopwatch.Elapsed}"
    printfn ""
    for solution in solutions do
        printfn $"{Puzzle.printBoard solution}"
