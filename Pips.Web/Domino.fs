namespace Pips.Web

open System
open Fable.Core.JsInterop
open Pips

module Domino =

    /// Largest pip count.
    let private maxPipCount = 6

    /// All dominoes sorted from smallest to largest.
    let private allDominoes =
        [|
            for sum in 0 .. maxPipCount * 2 do
                for left in 0 .. min sum maxPipCount do
                    let right = sum - left
                    yield Domino.create left right
        |]

    /// Maps each domino to its index.
    let private dominoMap =
        Map [
            for i = 0 to allDominoes.Length - 1 do

                let domino = allDominoes[i]
                yield domino, i

                let domino = Domino.create domino.Right domino.Left
                yield domino, i
        ]

    /// Determines color of the given domino.
    let private getDominoColor domino =
        let hue =
            360.0 * float dominoMap[domino] / float dominoMap.Count
        $"hsl({hue}, 100%%, 80%%)"

    /// Draws a single pip at the given location.
    let private drawPip (ctx : Context) radius x y =
        ctx.beginPath()
        ctx.arc(x, y, radius, 0, 2.0 * Math.PI)
        ctx.fillStyle <- !^"black"
        ctx.fill()

    /// Draws the pattern of the given pip count at the given
    /// location.
    (*
     *  ┌───────┐ ┌───────┐ ┌───────┐ ┌───────┐ ┌───────┐ ┌───────┐ ┌───────┐
     *  │       │ │       │ │ ⬤     │ │ ⬤     │ │ ⬤   ⬤ │ │ ⬤   ⬤ │ │ ⬤ ⬤ ⬤ │
     *  │       │ │   ⬤   │ │       │ │   ⬤   │ │       │ │   ⬤   │ │       │
     *  │       │ │       │ │     ⬤ │ │     ⬤ │ │ ⬤   ⬤ │ │ ⬤   ⬤ │ │ ⬤ ⬤ ⬤ │
     *  └───────┘ └───────┘ └───────┘ └───────┘ └───────┘ └───────┘ └───────┘
     *)
    let private drawPipCount ctx cellSize x y (value : PipCount) =

        let radius = cellSize / 12.0
        let offset = cellSize / 4.0

        let pip = drawPip ctx radius

        let pip1 () = pip x y

        let pip2 () =
            pip (x - offset) (y - offset)
            pip (x + offset) (y + offset)

        let pip4 () =
            pip2 ()
            pip (x - offset) (y + offset)
            pip (x + offset) (y - offset)

        let pip6 () =
            pip4 ()
            pip x (y - offset)
            pip x (y + offset)

        match value with
            | 0 -> ()
            | 1 -> pip1 ()
            | 2 -> pip2 ()
            | 3 -> pip1 (); pip2 ()
            | 4 -> pip4 ()
            | 5 -> pip1 (); pip4 ()
            | 6 -> pip6 ()
            | _ -> failwith "Unexpected"

    /// Length of one side of a cell.
    let cellSize = 35.0

    /// Outer border of a domino.
    let outerStyle = 2.0, "black"

    /// Inner divider of a domino.
    let innerStyle = 1.0, "gray"

    /// Draws the given domino horizontally at the origin.
    /// Callers use rotation and translatation to modify the
    /// domino's location.
    let private drawDomino (ctx : Context) domino =

            // draw the domino rectangle
        ctx.beginPath()
        ctx.roundRect(
            0, 0,
            cellSize * 2.0, cellSize,
            cellSize / 8.0)
        ctx.fillStyle <- !^(getDominoColor domino)
        ctx.fill()
        ctx.lineWidth <- fst outerStyle
        ctx.strokeStyle <- !^(snd outerStyle)
        ctx.stroke()

            // draw the dividing line
        ctx.beginPath()
        ctx.moveTo(cellSize, 0)
        ctx.lineTo(cellSize, cellSize)
        ctx.lineWidth <- fst innerStyle
        ctx.strokeStyle <- !^(snd innerStyle)
        ctx.stroke()

            // draw the pips
        let leftX = cellSize * 0.5
        let rightX = cellSize * 1.5
        let centerY = cellSize * 0.5
        drawPipCount ctx cellSize leftX centerY domino.Left
        drawPipCount ctx cellSize rightX centerY domino.Right

    /// Unplaced domino scale.
    let unplacedDominoScale = 3.0 / 4.0

    /// Draws the given unplaced domino at the given position.
    let drawUnplacedDomino ctx x y domino =
        use _ = Canvas.save ctx
        ctx.translate(x, y)
        ctx.scale(unplacedDominoScale, unplacedDominoScale)
        drawDomino ctx domino

    /// Draws the given placed domino at the given edge.
    let drawSolutionDomino ctx domino ((cellA, cellB) : Edge) =

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

            // apply transformations
        use _ = Canvas.save ctx
        ctx.translate(
            float (cellA.Column + colDiff) * cellSize,
            float (cellA.Row + rowDiff) * cellSize)
        ctx.rotate(float nTwists * Math.PI / 2.0)

            // draw domino
        drawDomino ctx domino
