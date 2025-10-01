namespace Pips

module Program =

    let puzzle =
        {
            UnplacedDominoes =
                [
                    { Left = 4; Right = 4 }
                    { Left = 3; Right = 5 }
                    { Left = 0; Right = 3 }
                    { Left = 2; Right = 2 }
                ]
            Regions =
                [|
                    {
                        Cells = [|
                            { Row = 0; Column = 1 } |]
                        Type = SumGreater 3
                    }
                    {
                        Cells = [|
                            { Row = 0; Column = 2 }
                            { Row = 1; Column = 1 }
                            { Row = 1; Column = 2 }
                            { Row = 2; Column = 2 } |]
                        Type = Unequal
                    }
                    {
                        Cells = [|
                            { Row = 1; Column = 0 } |]
                        Type = Unconstrained
                    }
                    {
                        Cells = [|
                            { Row = 1; Column = 3 } |]
                        Type = SumLess 3
                    }
                    {
                        Cells = [|
                            { Row = 2; Column = 1 } |]
                        Type = SumGreater 4
                    }
                |]
            Board = Board.empty
        }

    for solution in Puzzle.solve puzzle do
        Puzzle.printBoard solution
        printfn ""
