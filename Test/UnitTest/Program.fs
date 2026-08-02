namespace Pips.Test

open System
open System.Diagnostics

open FsCheck.FSharp

open Pips

module Program =

    let getCornerChar up down left right =
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

    let printPuzzle puzzle =

        let maxRow = puzzle.Board.NumRows - 1
        let maxCol = puzzle.Board.NumColumns - 1

        let cells =
            puzzle.Regions
                |> Seq.collect _.Cells
                |> set

        let regionMap =
            Map [
                for region in puzzle.Regions do
                    for cell in region.Cells do
                        yield cell, region
            ]

        let inSameRegion c1 c2 =
            Map.tryFind c1 regionMap = Map.tryFind c2 regionMap

        let isPresent cell =
            cells.Contains(cell)

        let hasHorizontalRegionBorder row col =
            let cell = Cell.create row col
            let topCell = Cell.create (row - 1) cell.Column
            (isPresent cell || isPresent topCell)
                && not (inSameRegion cell topCell)

        let hasVerticalRegionBorder row col =
            let cell = Cell.create row col
            let leftCell = Cell.create cell.Row (col - 1)
            (isPresent cell || isPresent leftCell)
                && not (inSameRegion cell leftCell)

        let getRegionCornerChar row col =
            let right = hasHorizontalRegionBorder row col
            let left = hasHorizontalRegionBorder row (col - 1)
            let down = hasVerticalRegionBorder row col
            let up = hasVerticalRegionBorder (row - 1) col
            getCornerChar up down left right

        let getRegionDisplay (region : Region) =
            match region.Type with
                | RegionType.Any -> "*"
                | RegionType.Equal -> "="
                | RegionType.Unequal -> "≠"
                | RegionType.SumLess n -> sprintf "<%d" n
                | RegionType.SumGreater n -> sprintf ">%d" n
                | RegionType.SumExact n -> sprintf "%d" n

        let regionDisplayMap =
            Map [
                for region in puzzle.Regions do
                    let cell = Seq.max region.Cells
                    let display = getRegionDisplay region
                    cell, display
            ]

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

        let maxRow = solution.Board.NumRows - 1
        let maxCol = solution.Board.NumColumns - 1

        let dominoMap =
            Map [
                for (_, (c1, c2)) in solution.Board.DominoPlaces do
                    let d =
                        min c1 c2,
                        max c1 c2
                    yield! [ c1, d; c2, d ]
            ]

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
            getCornerChar up down left right

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

        let trySolve (date : DateOnly) =
            let puzzleOpt =
                let dateStr = date.ToString("yyyy-MM-dd")
                Daily.loadHttp $"https://www.nytimes.com/svc/pips/v1/{dateStr}.json"
                    |> Map.tryFind "hard"
            match puzzleOpt with
                | Some puzzle ->
                    let stopwatch = Stopwatch.StartNew()
                    let solutions = Backtrack.solveEager puzzle
                    Ok (stopwatch.Elapsed.TotalSeconds, solutions)
                | None -> Error "Missing puzzle"

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
                Error "timeout"

        let print (date : DateOnly) = function
            | Ok (time : float, solutions) ->
                let solutions = Seq.toArray solutions
                printfn $"{date}: Found {solutions.Length} solution(s) in {time} seconds"
                printfn ""
                printfn $"{printSolution solutions[0]}"
            | Error msg ->
                printfn $"{date}: {msg}"
                printfn ""

        let startDate = DateOnly.Parse("8/18/2025")
        let endDate = DateOnly.Parse("11/25/2025")
        let lastOffset = endDate.DayNumber - startDate.DayNumber
        let pairs =
            [| 0 .. lastOffset |]
                |> Array.map (fun offset ->
                    let date = startDate.AddDays(offset)
                    let result =
                        run 150000 (fun () -> trySolve date)
                    print date result
                    Threading.Thread.Sleep(500)
                    date, result)
        for (date, result) in pairs do
            match result with
                | Ok (time, solutions) ->
                    printfn $"{date}, {time}, {Seq.length solutions}"
                | Error msg ->
                    printfn $"{date}, {msg}"

    let solveOne () =

            // download and print puzzle
        let puzzle =
            Daily.loadHttp "https://www.nytimes.com/svc/pips/v1/2025-11-13.json"
                |> Map.find "hard"
        printfn "Puzzle:"
        printfn ""
        printPuzzle puzzle
        printfn ""

            // solve puzzle and print solutions
        let stopwatch = Stopwatch.StartNew()
        let solutions = Backtrack.solveEager puzzle
        stopwatch.Stop()
        printfn $"Found {solutions.Length} solution(s) in {stopwatch.Elapsed}:"
        printfn ""
        for solution in solutions do
            printSolution solution
        stopwatch.Elapsed

    let solveAnother () =

            // download and print puzzle
        let puzzle =
            Daily.loadHttp "https://www.nytimes.com/svc/pips/v1/2025-09-15.json"
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

            let solutions = Backtrack.solveEager solved.Puzzle
            printfn $"Found {solutions.Length} solution(s):"
            printfn ""
            printSolution solutions[0]

    let searchPlacement lookahead puzzle =
        Placement.search lookahead puzzle
            |> Map.toSeq
            |> Seq.tryPick (fun (domino, placements) ->
                Seq.tryExactlyOne placements
                    |> Option.map (fun placement ->
                        lookahead, domino, placement))

    let explainPlacement lookahead domino placement puzzle =

        printfn $"{domino} must be placed at {placement.Edge}"

        let edges =
            let forward =
                puzzle
                    |> Puzzle.getAllTilings
                    |> Set.unionMany
            let reverse =
                Set.map Edge.reverse forward
            Set.union forward reverse

        let allReasons =
            [|
                for edge in edges do
                    match Puzzle.tryPlaceValid domino edge puzzle with
                        | Ok _ -> ()
                        | Error pairs ->
                            for pair in pairs do
                                pair, edge
            |]
                |> Array.groupBy fst
                |> Array.map (fun (pair, group) ->
                    pair, Array.map snd group)
                |> Array.sortByDescending (snd >> Array.length)

        let successEdges, reasons =
            ((edges, List.empty), allReasons)
                ||> Array.fold (
                    fun
                        (unusedEdges, acc)
                        ((region, result), usedEdges) ->
                        let usedEdges =
                            Set.intersect unusedEdges (set usedEdges)
                        let unusedEdges = unusedEdges - usedEdges
                        let acc = ((region, result), usedEdges) :: acc
                        unusedEdges, acc)

        assert(successEdges.Contains(placement.Edge))
        for edge in successEdges do
            printfn $"   Success: Edge {edge}"

        for ((region, result), edges) in List.rev reasons do
            printfn $"   Region {region.Cells.[0]} invalid because {result}"
            for edge in edges do
                printfn $"      Edge {edge}"

    let explainPuzzle puzzle =

        printPuzzle puzzle
        printfn ""

        [0; 1; 2]
            |> Seq.tryPick (fun lookahead ->
                searchPlacement lookahead puzzle)
            |> Option.iter (fun (lookahead, domino, placement) ->
                explainPlacement lookahead domino placement puzzle)

    let explainOne () =
        let puzzle =
            let dateStr = "2025-08-21"
            Daily.loadHttp $"https://www.nytimes.com/svc/pips/v1/{dateStr}.json"
                |> Map.find "easy"
        explainPuzzle puzzle

    System.Console.OutputEncoding <- System.Text.Encoding.UTF8
    explainOne ()
