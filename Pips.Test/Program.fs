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

        let dominoMap =
            puzzle.Board.Dominoes
                |> Seq.collect (fun (_, c1, c2) ->
                    let d = min c1 c2, max c1 c2
                    [ c1, d; c2, d ])
                |> Map

        let inSameDomino c1 c2 =
            Map.tryFind c1 dominoMap = Map.tryFind c2 dominoMap
        
        let isCellEmpty cell =
            if cell.Row < 0
                || cell.Row > maxRow
                || cell.Column < 0
                || cell.Column > maxCol then true
            else
                puzzle.Board[cell] = Board.empty

        for row in 0 .. maxRow do

                // print top border
            for col in 0 .. maxCol do
                let cell = Cell.create row col
                let topCell = { cell with Row = row - 1 }
                if (not (isCellEmpty cell) || not (isCellEmpty topCell))
                    && not (inSameDomino cell topCell) then
                    printf "+---"
                else
                    printf "+   "
            printfn "+"

                // print cell content and side borders
            for col in 0 .. maxCol do

                let cell = Cell.create row col

                let leftCell = { cell with Column = col - 1 }
                if (not (isCellEmpty cell) || not (isCellEmpty leftCell))
                    && not (inSameDomino cell leftCell) then
                    printf "|"
                else
                    printf " "
                
                match puzzle.Board[cell] with
                    | Board.empty -> printf "   "
                    | v -> printf " %d " v
            
            let rightCell = Cell.create row (maxCol + 1)
            let lastCell = Cell.create row maxCol
            if (not (isCellEmpty lastCell) || not (isCellEmpty rightCell))
                && not (inSameDomino lastCell rightCell) then
                printfn "|"
            else
                printfn ""

            // print bottom border for the last row
        for col in 0 .. maxCol do
            let cell = Cell.create maxRow col
            let bottomCell = { cell with Row = maxRow + 1 }
            if (not (isCellEmpty cell) || not (isCellEmpty bottomCell))
                && not (inSameDomino cell bottomCell) then
                printf "+---"
            else
                printf "+   "
        printfn "+"

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

    solveOne ()
