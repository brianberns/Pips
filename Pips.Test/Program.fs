namespace Pips

open System
open System.Diagnostics
open System.Threading

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


    let run timeout work =
        let work =
            async {
                let! child =
                    Async.StartChild(
                        async { return work () },
                        timeout)
                return! child
            }
        try
            Async.RunSynchronously(work, timeout)
        with :? TimeoutException ->
            printfn "Timeout"
            printfn ""

    let startDate = DateOnly.Parse("9/1/2025")
    for offset = 0 to 30 do
        let work () =
            solve (startDate.AddDays(offset))
        run 10000 work
    printfn "End"
