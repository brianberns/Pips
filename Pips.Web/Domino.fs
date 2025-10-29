namespace Pips.Web

open System
open Fable.Core.JsInterop
open Pips

module Domino =

    let private maxPipCount = 6

    let private allDominoes =
        [|
            for sum in 0 .. maxPipCount * 2 do
                for left in 0 .. min sum maxPipCount do
                    let right = sum - left
                    yield Domino.create left right
        |]

    let private dominoMap =
        Map [
            for i = 0 to allDominoes.Length - 1 do

                let domino = allDominoes[i]
                yield domino, i

                let domino = Domino.create domino.Right domino.Left
                yield domino, i
        ]

    let private getDominoColor domino =
        let hue =
            360.0 * float dominoMap[domino] / float dominoMap.Count
        $"hsl({hue}, 100%%, 80%%)"

    let private drawPip (ctx : Context) radius x y =
        ctx.beginPath()
        ctx.arc(x, y, radius, 0, 2.0 * Math.PI)
        ctx.fillStyle <- !^"black"
        ctx.fill()

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

    let outerStyle = 2.0, "black"
    let innerStyle = 1.0, "gray"

    /// Draws the given domino horizontally at the origin.
    /// Callers use rotation and translatation to modify the
    /// domino's location.
    let private drawDomino (ctx : Context) cellSize domino =

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

    let cellSize = 40.0

    /// Draws the given unplaced domino at the given position.
    let private drawUnplacedDomino (ctx : Context) x y domino =

        ctx.save()

        ctx.translate(x, y)
        let scale = 2.0 / 3.0
        ctx.scale(scale, scale)

        drawDomino ctx cellSize domino

        ctx.restore()

    /// Draws the given unplaced dominoes starting at the given
    /// Y position.
    let drawUnplacedDominoes (ctx : Context) startY dominoes =
        let dominoChunks =
            dominoes
                |> Seq.chunkBySize 4 
                |> Seq.toArray
        for row = 0 to dominoChunks.Length - 1 do
            let dominoChunk = dominoChunks[row]
            for col = 0 to dominoChunk.Length - 1 do
                let domino = dominoChunk[col]
                let x = float col * cellSize * 2.0
                let y = startY + (float row * cellSize)
                drawUnplacedDomino ctx x y domino

    /// Draws the given placed domino at the given edge.
    let drawSolutionDomino
        (ctx : Context) domino ((cellA, cellB) : Edge) =

        let row, col, nTwists =
            match cellB.Row - cellA.Row, cellB.Column - cellA.Column with
                |  0,  1 -> cellA.Row,     cellA.Column,     0   // horizontal
                |  1,  0 -> cellA.Row,     cellA.Column + 1, 1   // vertical
                |  0, -1 -> cellA.Row + 1, cellA.Column + 1, 2   // horizontal flipped
                | -1,  0 -> cellA.Row + 1, cellA.Column,     3   // vertical flipped
                | _ -> failwith "Unexpected"

        ctx.save()
        ctx.translate(
            float col * cellSize,
            float row * cellSize)
        ctx.rotate(float nTwists * Math.PI / 2.0)

        drawDomino ctx cellSize domino

        ctx.restore()
