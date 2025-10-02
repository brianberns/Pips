namespace Pips

module Program =

    let puzzleMap =
        Daily.parse "https://www.nytimes.com/svc/pips/v1/2025-09-30.json"
    for name in [ "easy"; "medium"; "hard" ] do
        printfn ""
        printfn $"{name}:"
        for solution in Puzzle.solve puzzleMap[name] do
            printfn $"{Puzzle.printBoard solution}"
