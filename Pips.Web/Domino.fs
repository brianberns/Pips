namespace Pips.Web

open Feliz

open Pips

module Domino =

    /// Pips of each pip count, as indexes into a 3x3 grid,
    /// numbered in reading order.
    (*
     *  ┌───────┐ ┌───────┐ ┌───────┐ ┌───────┐ ┌───────┐ ┌───────┐ ┌───────┐
     *  │       │ │       │ │ ⬤     │ │ ⬤     │ │ ⬤   ⬤ │ │ ⬤   ⬤ │ │ ⬤ ⬤ ⬤ │
     *  │       │ │   ⬤   │ │       │ │   ⬤   │ │       │ │   ⬤   │ │       │
     *  │       │ │       │ │     ⬤ │ │     ⬤ │ │ ⬤   ⬤ │ │ ⬤   ⬤ │ │ ⬤ ⬤ ⬤ │
     *  └───────┘ └───────┘ └───────┘ └───────┘ └───────┘ └───────┘ └───────┘
     *)
    let private pipGrids =
        [|
            []                       // 0
            [4]                      // 1
            [0; 8]                   // 2
            [0; 4; 8]                // 3
            [0; 2; 6; 8]             // 4
            [0; 2; 4; 6; 8]          // 5
            [0; 1; 2; 6; 7; 8]       // 6
        |]

    /// Renders one half of a domino.
    let private renderHalf (pipCount : PipCount) =
        Html.div [
            prop.className "domino-half"
            prop.children [
                for iPip in pipGrids[pipCount] do
                    Html.div [
                        prop.key iPip
                        prop.className "pip"
                        prop.style [
                            style.gridRowStart (iPip / 3 + 1)
                            style.gridColumnStart (iPip % 3 + 1)
                        ]
                    ]
            ]
        ]

    /// Renders the given domino with the given additional
    /// classes and styles.
    let private renderDomino key classes styles domino =
        Html.div [
            prop.key (key : string)
            prop.classes [
                yield "domino"
                yield! classes
            ]
            prop.style styles
            prop.children [
                renderHalf domino.Left
                renderHalf domino.Right
            ]
        ]

    /// Renders the given domino as one that has not yet been
    /// placed on a board.
    let renderUnplaced domino =
        renderDomino $"{domino}" [] [] domino

    /// Renders the given domino placed at the given edge. The
    /// domino is laid out horizontally, then rotated a quarter
    /// turn at a time into position.
    let renderPlaced domino ((cellA, cellB) : Edge) =

            // determine domino orientation
        let rowDiff, colDiff, nTwists =
            let rowDiff = cellB.Row - cellA.Row
            let colDiff = cellB.Column - cellA.Column
            match rowDiff, colDiff with
                |  0,  1 -> 0, 0, 0   // horizontal
                |  1,  0 -> 0, 1, 1   // vertical
                |  0, -1 -> 1, 1, 2   // horizontal flipped
                | -1,  0 -> 1, 0, 3   // vertical flipped
                | _ -> failwith "Unexpected"

        renderDomino
            $"{cellA}-{cellB}"
            [ "domino-placed" ]
            [
                style.custom ("--row", $"{cellA.Row + rowDiff}")
                style.custom ("--col", $"{cellA.Column + colDiff}")
                style.custom ("--twists", $"{nTwists}")
            ]
            domino
