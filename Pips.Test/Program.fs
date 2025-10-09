namespace Pips

open System
open System.Diagnostics

module Program =

    let solve (date : DateOnly) =
        let puzzleMap =
            let dateStr = date.ToString("yyyy-MM-dd")
            Daily.loadHttp $"https://www.nytimes.com/svc/pips/v1/{dateStr}.json"
        let stopwatch = Stopwatch.StartNew()
        let solutions = Puzzle.solve puzzleMap["hard"]
        stopwatch.Elapsed.TotalSeconds, solutions

    let printBoard puzzle =
        let maxRow =
            puzzle.Regions
                |> Array.collect _.Cells
                |> Array.map _.Row
                |> Array.max
        let maxCol =
            puzzle.Regions
                |> Array.collect _.Cells
                |> Array.map _.Column
                |> Array.max
        for row in 0 .. maxRow do
            for col in 0 .. maxCol do
                let cell = { Row = row; Column = col }
                match puzzle.Board[cell] with
                    | Board.empty -> printf "  "
                    | v -> printf $"{v} "
            printfn ""

    let solveMany () =

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

        let print date resultOpt =
            match resultOpt with
                | Some (time, (puzzles : List<_>)) ->
                    printfn $"{date}: {time} seconds, {puzzles.Length} solution(s)"
                    printfn ""
                    printfn $"{printBoard puzzles[0]}"
                | None ->
                    printfn $"{date}: timeout"

        let startDate = DateOnly.Parse("8/18/2025")
        let date, resultOpt =
            [ 0 .. 80 ]
                |> Seq.map (fun offset ->
                    let date = startDate.AddDays(offset)
                    let resultOpt =
                        run 20000 (fun () -> solve date)
                    print date resultOpt
                    Threading.Thread.Sleep(500)
                    date, resultOpt)
                |> Seq.maxBy snd
        printfn "Longest:"
        print date resultOpt

    let solveOne () =
        let puzzleMap = Daily.loadHttp "https://www.nytimes.com/svc/pips/v1/2025-09-05.json"
        let stopwatch = Stopwatch.StartNew()
        let solutions = Puzzle.solve puzzleMap["hard"]
        printfn $"Found {solutions.Length} solution(s) in {stopwatch.Elapsed}"
        printfn ""
        for solution in solutions do
            printfn $"{printBoard solution}"

    solveMany ()
