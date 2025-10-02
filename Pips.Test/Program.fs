namespace Pips

open System
open System.Diagnostics
open System.Threading

module Program =

    let solve (date : DateOnly) =
        let puzzleMap =
            let dateStr = date.ToString("yyyy-MM-dd")
            Daily.loadHttp $"https://www.nytimes.com/svc/pips/v1/{dateStr}.json"
        let stopwatch = Stopwatch.StartNew()
        let _solutions = Puzzle.solve puzzleMap["hard"]
        stopwatch.Elapsed.TotalSeconds

    let run timeout work =
        let work =
            async {
                let! child =
                    Async.StartChild(
                        async { return Some (work ()) },
                        timeout)
                return! child
            }
        try
            Async.RunSynchronously(work, timeout)
        with :? TimeoutException ->
            None

    let startDate = DateOnly.Parse("9/1/2025")
    [ 0 .. 30 ]
        |> Seq.map (fun offset ->
            let date = startDate.AddDays(offset)
            let work () = solve date
            let timeOpt = run 10000 work
            printfn $"{date}: {timeOpt}"
            date, timeOpt)
        |> Seq.maxBy snd
        |> printfn "%A"
