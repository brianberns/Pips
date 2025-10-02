namespace Pips

open System.Diagnostics

module Program =

    let puzzleMap = Daily.loadFile "Daily.json"

    for name in [ "easy"; "medium"; "hard" ] do
        printfn ""
        printfn $"{name}:"
        printfn ""
        let stopwatch = Stopwatch.StartNew()
        let solutions = Puzzle.solve puzzleMap[name]
        printfn $"Found {solutions.Length} solution(s) in {stopwatch.Elapsed}"
        printfn ""
        for solution in solutions do
            printfn $"{Puzzle.printBoard solution}"
