namespace Pips.Web

open Fable.Core.JsInterop
open Pips

module Domino =

    let private maxPipCount = 6

    let private allDominoes =
        [|
            for left in 0 .. maxPipCount do
                for right in left .. maxPipCount do
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
        ctx.arc(x, y, radius, 0, 2.0 * System.Math.PI)
        ctx.fillStyle <- !^"black"
        ctx.fill()

    let private drawPipCount ctx cellSize x y (value : PipCount) =
        let radius = cellSize / 12.0
        let offset = cellSize / 4.0
        let pip = drawPip ctx radius
        match value with
            | 0 -> ()
            | 1 -> pip x y
            | 2 ->
                pip (x - offset) (y - offset)
                pip (x + offset) (y + offset)
            | 3 ->
                pip  x            y
                pip (x - offset) (y - offset)
                pip (x + offset) (y + offset)
            | 4 ->
                pip (x - offset) (y - offset)
                pip (x - offset) (y + offset)
                pip (x + offset) (y - offset)
                pip (x + offset) (y + offset)
            | 5 ->
                pip  x            y
                pip (x - offset) (y - offset)
                pip (x - offset) (y + offset)
                pip (x + offset) (y - offset)
                pip (x + offset) (y + offset)
            | 6 ->
                pip (x - offset) (y - offset)
                pip (x - offset) (y + offset)
                pip  x           (y - offset)
                pip  x           (y + offset)
                pip (x + offset) (y - offset)
                pip (x + offset) (y + offset)
            | _ -> failwith "Unexpected"

    let cellSize = 40.0
    let outerStyle = 2.0, "black"
    let innerStyle = 1.0, "gray"

    /// Draws the given unplaced domino at the given position.
    let private drawUnplacedDomino (ctx : Context) x y domino =

        let scale = 2.0 / 3.0
        let cellSize = cellSize * scale

        ctx.beginPath()
        ctx.roundRect(
            x, y,
            cellSize * 2.0, cellSize,
            cellSize / 8.0)
        ctx.fillStyle <- !^(getDominoColor domino)
        ctx.fill()
        ctx.lineWidth <- fst outerStyle * scale
        ctx.strokeStyle <- !^(snd outerStyle)
        ctx.stroke()

        ctx.beginPath()
        ctx.moveTo(x + cellSize, y)
        ctx.lineTo(x + cellSize, y + cellSize)
        ctx.lineWidth <- fst innerStyle * scale
        ctx.strokeStyle <- !^(snd innerStyle)
        ctx.stroke()

        let leftX = x + (cellSize * 0.5)
        let rightX = x + (cellSize * 1.5)
        let centerY = y + (cellSize * 0.5)
        drawPipCount ctx cellSize leftX centerY domino.Left
        drawPipCount ctx cellSize rightX centerY domino.Right

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

    let private drawSolutionPipCount ctx cell pipCount =
        let x = (float cell.Column + 0.5) * cellSize
        let y = (float cell.Row + 0.5) * cellSize
        drawPipCount ctx cellSize x y pipCount

    /// Draws the given placed domino at the given edge.
    let drawSolutionDomino
        (ctx : Context) domino ((cellA, cellB) : Edge) =

        ctx.beginPath()

        let x =
            float (min cellA.Column cellB.Column) * cellSize
        let y =
            float (min cellA.Row cellB.Row) * cellSize

        let isHorizontal = cellA.Row = cellB.Row
        let width =
            if isHorizontal then cellSize * 2.0
            else cellSize
        let height =
            if isHorizontal then cellSize
            else cellSize * 2.0
        ctx.roundRect(
            x, y,
            width, height,
            cellSize / 8.0)
        ctx.fillStyle <- !^(getDominoColor domino)
        ctx.fill()
        ctx.lineWidth <- fst outerStyle
        ctx.strokeStyle <- !^(snd outerStyle)
        ctx.stroke()

        ctx.beginPath()
        if isHorizontal then
            ctx.moveTo(x + cellSize, y)
            ctx.lineTo(x + cellSize, y + cellSize)
        else
            ctx.moveTo(x, y + cellSize)
            ctx.lineTo(x + cellSize, y + cellSize)
        ctx.lineWidth <- fst innerStyle
        ctx.strokeStyle <- !^(snd innerStyle)
        ctx.stroke()

        drawSolutionPipCount ctx cellA domino.Left
        drawSolutionPipCount ctx cellB domino.Right
