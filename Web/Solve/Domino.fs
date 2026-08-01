namespace Pips.Web

open Feliz
open Pips

module Domino =

    /// Largest pip count.
    let private maxPipCount = 6

    /// All dominoes sorted from smallest to largest.
    let private allDominoes =
        [|
            for left in 0 .. maxPipCount do
                for right in 0 .. left do
                    Domino.create left right
        |] |> Array.sortBy (fun domino ->
            domino.Left + domino.Right)

    /// Maps each domino to its index.
    let private dominoMap =
        Map [
            for i = 0 to allDominoes.Length - 1 do

                let domino = allDominoes[i]
                yield domino, i

                let domino = Domino.create domino.Right domino.Left
                yield domino, i
        ]

    /// Determines the hue of the given domino, in degrees.
    let private getHue domino =
        360.0 * float dominoMap[domino] / float allDominoes.Length

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
            prop.style [
                yield style.custom ("--hue", $"{getHue domino}")
                yield! styles
            ]
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
