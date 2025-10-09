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

        let hasHorizontalBorder row col =
            let cell = Cell.create row col
            let topCell = Cell.create (row - 1) cell.Column
            (not (isCellEmpty cell) || not (isCellEmpty topCell))
                && not (inSameDomino cell topCell)

        let hasVerticalBorder row col =
            let cell = Cell.create row col
            let leftCell = Cell.create cell.Row (col - 1)
            (not (isCellEmpty cell) || not (isCellEmpty leftCell))
                && not (inSameDomino cell leftCell)

        let getCornerChar row col =
            let right = hasHorizontalBorder row col
            let left = hasHorizontalBorder row (col - 1)
            let down = hasVerticalBorder row col
            let up = hasVerticalBorder (row - 1) col

            match up, down, left, right with
                | false, false, true, true -> "─"
                | true, true, false, false -> "│"
                | false, true, false, true -> "┌"
                | false, true, true, false -> "┐"
                | true, false, false, true -> "└"
                | true, false, true, false -> "┘"
                | false, true, true, true -> "┬"
                | true, false, true, true -> "┴"
                | true, true, false, true -> "├"
                | true, true, true, false -> "┤"
                | true, true, true, true -> "┼"
                | true, false, false, false -> "│"
                | false, true, false, false -> "│"
                | false, false, true, false -> "─"
                | false, false, false, true -> "─"
                | _ -> " "

        for row in 0 .. maxRow do

                // print top border line
            for col in 0 .. maxCol do
                printf "%s" (getCornerChar row col)
                if hasHorizontalBorder row col then
                    printf "───"
                else
                    printf "   "
            printfn "%s" (getCornerChar row (maxCol + 1))

                // print cell content and vertical borders
            for col in 0 .. maxCol do
                if hasVerticalBorder row col then
                    printf "│"
                else
                    printf " "
                
                let cell = Cell.create row col
                match puzzle.Board[cell] with
                    | Board.empty -> printf "   "
                    | v -> printf " %d " v
            
            if hasVerticalBorder row (maxCol + 1) then
                printfn "│"
            else
                printfn ""

            // print bottom border for the last row
        for col in 0 .. maxCol do
            printf "%s" (getCornerChar (maxRow + 1) col)
            if hasHorizontalBorder (maxRow + 1) col then
                printf "───"
            else
                printf "   "
        printfn "%s" (getCornerChar (maxRow + 1) (maxCol + 1))

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
        let puzzleMap = Daily.loadHttp "https://www.nytimes.com/svc/pips/v1/2025-10-14.json"
        let stopwatch = Stopwatch.StartNew()
        let solutions = Puzzle.solve puzzleMap["hard"]
        printfn $"Found {solutions.Length} solution(s) in {stopwatch.Elapsed}"
        printfn ""
        for solution in solutions do
            printfn $"{printBoard solution}"

    solveOne ()
