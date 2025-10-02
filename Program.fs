namespace Pips

module Program =

    let puzzleMap =
        Daily.parse "https://www.nytimes.com/svc/pips/v1/2025-09-30.json"
    for name in [ "easy"; "medium"; "hard" ] do
        printfn ""
        printfn $"{name}:"
        printfn ""
        let solutions = Puzzle.solve puzzleMap[name]
        printfn $"{solutions.Length} solution(s)"
        printfn ""
        for solution in solutions do
            printfn $"{Puzzle.printBoard solution}"
