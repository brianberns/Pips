namespace Pips.Fuzz

open FsCheck.FSharp
open FsCheck.Xunit

open Pips

type Edge = Cell * Cell

module Puzzle =

    let allDominoes =
        let range = [| PipCount.minValue .. PipCount.maxValue |]
        (range, range)
            ||> Array.map2 Domino.create
            |> set

    let emptyBoard = Board.create 10 10

    let allEmptyEdges : Set<Edge> =
        set [
            for row = 0 to emptyBoard.Cells.GetLength(0) - 2 do
                for col = 0 to emptyBoard.Cells.GetLength(1) - 2 do
                    let cellA = Cell.create row col
                    let cellB = Cell.create (row + 1) col
                    let cellC = Cell.create row (col + 1)
                    yield cellA, cellB   // down
                    yield cellA, cellC   // right
        ]

    let rec place (emptyEdges : Set<_>) puzzle =
        gen {
            if puzzle.UnplacedDominoes.IsEmpty then
                return puzzle
            else
                let! domino = Gen.elements puzzle.UnplacedDominoes
                let! edge : Edge = Gen.elements emptyEdges
                let emptyEdges = emptyEdges.Remove(edge)
                let! flag = Gen.elements [ true; false ]
                let cellLeft, cellRight =
                    if flag then fst edge, snd edge
                    else snd edge, fst edge
                let puzzle = Puzzle.place domino cellLeft cellRight puzzle
                return! place emptyEdges puzzle
        }

    let gen =
        gen {
            let! dominoes =
                allDominoes
                    |> Gen.subListOf
                    |> Gen.map set
            let! puzzle =
                place allEmptyEdges {
                    UnplacedDominoes = dominoes
                    Regions = Array.empty
                    Board = emptyBoard
                }
            let cells =
                puzzle.Board.Dominoes
                    |> Seq.collect (fun (_, cellA, cellB) ->
                        [ cellA; cellB ])
                    |> Seq.toArray
            let region =
                {
                    Cells = cells
                    Type = RegionType.Any
                }
            return { puzzle with Regions = [| region |] }
        }
        
    let arb = Arb.fromGen gen

type Generators =
    static member Puzzle() = Puzzle.arb

module Generators =

    [<assembly: Properties(
        Arbitrary = [| typeof<Generators> |],
        MaxTest = 1000)>]
    do ()

module Fuzz =

    [<Property>]
    let ``Solvable`` puzzle =
        Puzzle.trySolve puzzle |> Option.isSome

