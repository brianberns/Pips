namespace Pips

open System
open System.Diagnostics

module Program =

    let solve (date : DateOnly) =

        printfn $"{date}"

        let puzzleMap =
            let dateStr = date.ToString("yyyy-MM-dd")
            Daily.loadHttp $"https://www.nytimes.com/svc/pips/v1/{dateStr}.json"

        let stopwatch = Stopwatch.StartNew()
        let solutions = Puzzle.solve puzzleMap["hard"]
        printfn $"Found {solutions.Length} solution(s) in {stopwatch.Elapsed}:"
        printfn ""
        printfn $"{Puzzle.printBoard solutions[0]}"

    let startDate = DateOnly.Parse("9/1/2025")
    for offset = 0 to 30 do
        solve (startDate.AddDays(offset))
