namespace Pips

module Program =

    (*
    for solution in Puzzle.solve puzzle do
        Puzzle.printBoard solution
        printfn ""
    *)
    let moo = Daily.parse "https://www.nytimes.com/svc/pips/v1/2025-09-30.json"
    printfn "%A" moo
