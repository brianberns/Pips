namespace Pips

open System
open System.Diagnostics

open FsCheck.FSharp

module Program =

    let loadPuzzles uri =
        (Daily.loadHttp uri).Result

    let printPuzzle puzzle =

        let cells =
            puzzle.Regions
                |> Array.collect _.Cells

        let maxRow =
            if Array.isEmpty cells then 0
            else
                cells
                    |> Array.map _.Row
                    |> Array.max

        let maxCol =
            if Array.isEmpty cells then 0
            else
                cells
                    |> Array.map _.Column
                    |> Array.max

        let regionMap =
            puzzle.Regions
                |> Seq.collect (fun region ->
                    region.Cells
                        |> Seq.map (fun cell ->
                            cell, region))
                |> Map

        let inSameRegion c1 c2 =
            Map.tryFind c1 regionMap = Map.tryFind c2 regionMap

        let cellSet =
            puzzle.Regions
                |> Array.collect _.Cells
                |> set

        let isCellEmpty cell =
            not (cellSet.Contains(cell))

        let hasHorizontalRegionBorder row col =
            let cell = Cell.create row col
            let topCell = Cell.create (row - 1) cell.Column
            (not (isCellEmpty cell) || not (isCellEmpty topCell))
                && not (inSameRegion cell topCell)

        let hasVerticalRegionBorder row col =
            let cell = Cell.create row col
            let leftCell = Cell.create cell.Row (col - 1)
            (not (isCellEmpty cell) || not (isCellEmpty leftCell))
                && not (inSameRegion cell leftCell)

        let getRegionCornerChar row col =
            let right = hasHorizontalRegionBorder row col
            let left = hasHorizontalRegionBorder row (col - 1)
            let down = hasVerticalRegionBorder row col
            let up = hasVerticalRegionBorder (row - 1) col

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

        let getRegionDisplay (region : Region) =
            match region.Type with
                | RegionType.Any -> "*"
                | RegionType.Equal -> "="
                | RegionType.Unequal -> "≠"
                | RegionType.SumLess n -> sprintf "<%d" n
                | RegionType.SumGreater n -> sprintf ">%d" n
                | RegionType.SumExact n -> sprintf "%d" n

        let regionDisplayMap =
            puzzle.Regions
                |> Array.map (fun region ->
                    let cell = Seq.max region.Cells
                    let display = getRegionDisplay region
                    cell, display)
                |> Map

        for row in 0 .. maxRow do

                // print top border line
            for col in 0 .. maxCol do
                printf "%s" (getRegionCornerChar row col)
                if hasHorizontalRegionBorder row col then
                    printf "───"
                else
                    printf "   "
            printfn "%s" (getRegionCornerChar row (maxCol + 1))

                // print cell content and vertical borders
            for col in 0 .. maxCol do
                if hasVerticalRegionBorder row col then
                    printf "│"
                else
                    printf " "
                
                let cell = Cell.create row col
                match Map.tryFind cell regionDisplayMap with
                    | Some display ->
                        let padding = max 0 (3 - display.Length)
                        let leftPadding = padding / 2
                        let rightPadding = padding - leftPadding
                        printf "%s%s%s" (String(' ', leftPadding)) display (String(' ', rightPadding))
                    | None -> printf "   "
            
            if hasVerticalRegionBorder row (maxCol + 1) then
                printfn "│"
            else
                printfn ""

            // print bottom border for the last row
        for col in 0 .. maxCol do
            printf "%s" (getRegionCornerChar (maxRow + 1) col)
            if hasHorizontalRegionBorder (maxRow + 1) col then
                printf "───"
            else
                printf "   "
        printfn "%s" (getRegionCornerChar (maxRow + 1) (maxCol + 1))

            // print dominoes
        printfn ""
        printfn "Dominoes:"
        printfn ""
        for chunk in Seq.chunkBySize 7 puzzle.UnplacedDominoes do
            for domino in chunk do
                printf "%d-%d   " domino.Left domino.Right
            printfn ""

    let printSolution solution =

        let cells =
            solution.Regions
                |> Array.collect _.Cells

        let maxRow =
            if Array.isEmpty cells then 0
            else
                cells
                    |> Array.map _.Row
                    |> Array.max

        let maxCol =
            if Array.isEmpty cells then 0
            else
                cells
                    |> Array.map _.Column
                    |> Array.max

        let dominoMap =
            solution.Board.DominoPlaces
                |> Seq.collect (fun (_, (c1, c2)) ->
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
                solution.Board[cell] = Board.emptyCell

        let hasHorizontalDominoBorder row col =
            let cell = Cell.create row col
            let topCell = Cell.create (row - 1) cell.Column
            (not (isCellEmpty cell) || not (isCellEmpty topCell))
                && not (inSameDomino cell topCell)

        let hasVerticalDominoBorder row col =
            let cell = Cell.create row col
            let leftCell = Cell.create cell.Row (col - 1)
            (not (isCellEmpty cell) || not (isCellEmpty leftCell))
                && not (inSameDomino cell leftCell)

        let getDominoCornerChar row col =
            let right = hasHorizontalDominoBorder row col
            let left = hasHorizontalDominoBorder row (col - 1)
            let down = hasVerticalDominoBorder row col
            let up = hasVerticalDominoBorder (row - 1) col

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
                printf "%s" (getDominoCornerChar row col)
                if hasHorizontalDominoBorder row col then
                    printf "───"
                else
                    printf "   "
            printfn "%s" (getDominoCornerChar row (maxCol + 1))

                // print cell content and vertical borders
            for col in 0 .. maxCol do
                if hasVerticalDominoBorder row col then
                    printf "│"
                else
                    printf " "
                
                let cell = Cell.create row col
                match solution.Board[cell] with
                    | Board.emptyCell -> printf "   "
                    | v -> printf " %d " v
            
            if hasVerticalDominoBorder row (maxCol + 1) then
                printfn "│"
            else
                printfn ""

            // print bottom border for the last row
        for col in 0 .. maxCol do
            printf "%s" (getDominoCornerChar (maxRow + 1) col)
            if hasHorizontalDominoBorder (maxRow + 1) col then
                printf "───"
            else
                printf "   "
        printfn "%s" (getDominoCornerChar (maxRow + 1) (maxCol + 1))

    let solveMany () =

        let solve (date : DateOnly) =
            let puzzle =
                let dateStr = date.ToString("yyyy-MM-dd")
                loadPuzzles $"https://www.nytimes.com/svc/pips/v1/{dateStr}.json"
                    |> Map.find "hard"
            let stopwatch = Stopwatch.StartNew()
            let solutions = Backtrack.solve puzzle
            stopwatch.Elapsed.TotalSeconds, solutions

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

        let print (date : DateOnly) = function
            | Some (time : float, solutions : _[]) ->
                printfn $"{date}: Found {solutions.Length} solution(s) in {time} seconds"
                printfn ""
                printfn $"{printSolution solutions[0]}"
            | None ->
                printfn $"{date}: timeout"
                printfn ""

        let startDate = DateOnly.Parse("8/18/2025")
        let pairs =
            [| 0 .. 87 |]
                |> Array.map (fun offset ->
                    let date = startDate.AddDays(offset)
                    let resultOpt =
                        run 150000 (fun () -> solve date)
                    print date resultOpt
                    Threading.Thread.Sleep(500)
                    date, resultOpt)
        for (date, resultOpt) in pairs do
            match resultOpt with
                | Some (time, solutions) ->
                    printfn $"{date}, {time}, {solutions.Length}"
                | None ->
                    printfn $"{date}, timeout"

    let solveOne () =

            // download and print puzzle
        let puzzle =
            loadPuzzles "https://www.nytimes.com/svc/pips/v1/2025-10-14.json"
                |> Map.find "hard"
        printfn "Puzzle:"
        printfn ""
        printPuzzle puzzle
        printfn ""

            // solve puzzle and print solutions
        let stopwatch = Stopwatch.StartNew()
        let solutions = Backtrack.solve puzzle
        stopwatch.Stop()
        printfn $"Found {solutions.Length} solution(s) in {stopwatch.Elapsed}:"
        printfn ""
        for solution in solutions do
            printSolution solution
        stopwatch.Elapsed

    let solveAnother () =

            // download and print puzzle
        let puzzle =
            loadPuzzles "https://www.nytimes.com/svc/pips/v1/2025-09-15.json"
                |> Map.find "hard"
        printfn "Puzzle:"
        printfn ""
        printPuzzle puzzle
        printfn ""

            // solve puzzle and print solutions
        let stopwatch = Stopwatch.StartNew()
        let solutions = Backtrack.trySolve puzzle |> Option.toArray
        stopwatch.Stop()
        printfn $"Found {solutions.Length} solution(s) in {stopwatch.Elapsed}:"
        printfn ""
        for solution in solutions do
            printSolution solution
        stopwatch.Elapsed

    let solveTwo () =
        let timeSpanA = solveOne ()
        let timeSpanB = solveAnother ()
        printfn ""
        printfn $"Total: {timeSpanA + timeSpanB}"

    let generate () =

        let samples =
            Gen.sample 1000 SolvedPuzzle.gen

        for solved in samples do
            printfn "----------------------------------------------------------------------"
            printfn ""
            printfn "Puzzle:"
            printfn ""
            printSolution solved.Solution
            printfn ""

            for region in solved.Solution.Regions do
                printfn $"{region.Type}: {region.Cells.Length} cells"
            printfn ""

            let solutions = Backtrack.solve solved.Puzzle
            printfn $"Found {solutions.Length} solution(s):"
            printfn ""
            printfn $"{printSolution solutions[0]}"

    System.Console.OutputEncoding <- System.Text.Encoding.UTF8
    solveMany ()
