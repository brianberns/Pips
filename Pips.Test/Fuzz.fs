namespace Pips

open FsCheck.FSharp
open FsCheck.Xunit

open Pips

type SolvedPuzzle =
    {
        Puzzle : Puzzle
        Solution : Puzzle
    }

module SolvedPuzzle =

    let allDominoes =
        let range = [| PipCount.minValue .. PipCount.maxValue |]
        (range, range)
            ||> Array.map2 Domino.create
            |> set

    let emptyBoard = Board.create 10 10

    let allEmptyEdges : Edge[] =
        [|
            for row = 0 to emptyBoard.Cells.GetLength(0) - 2 do
                for col = 0 to emptyBoard.Cells.GetLength(1) - 2 do
                    let cellA = Cell.create row col
                    let cellB = Cell.create (row + 1) col
                    let cellC = Cell.create row (col + 1)
                    yield cellA, cellB   // down
                    yield cellA, cellC   // right
        |]

    let rec place emptyEdges puzzle =
        gen {
            if puzzle.UnplacedDominoes.IsEmpty then
                return puzzle
            else
                let! domino = Gen.elements puzzle.UnplacedDominoes
                let! edge : Edge = Gen.elements emptyEdges
                let emptyEdges =
                    emptyEdges
                        |> Array.where (fun (cellA, cellB) ->
                            not (Edge.contains cellA edge)
                                && not (Edge.contains cellB edge))
                let! flag = Gen.elements [ true; false ]
                let edge =
                    if domino.Left = domino.Right || flag then edge
                    else Edge.reverse edge
                let puzzle =
                    Puzzle.place domino edge puzzle
                return! place emptyEdges puzzle
        }

    let gen =
        gen {
            let! dominoes =
                Gen.subListOf allDominoes
            let puzzle =
                {
                    UnplacedDominoes = set dominoes
                    Regions = Array.empty
                    Board = emptyBoard
                }
            let! solution = place allEmptyEdges puzzle
            let cells =
                solution.Board.DominoPlaces
                    |> Seq.collect (fun (_, (cellA, cellB)) ->
                        [ cellA; cellB ])
                    |> Seq.toArray
            let region =
                {
                    Cells = cells
                    Type = RegionType.Any
                }
            let puzzle =
                { puzzle with
                    Regions = [| region |] }
            let solution =
                { solution with
                    Regions = [| region |] }
            return {
                Puzzle = puzzle
                Solution = solution
            }
        }
        
    let arb = Arb.fromGen gen

type Generators =
    static member SolvedPuzzle() = SolvedPuzzle.arb

module Generators =

    [<assembly: Properties(
        Arbitrary = [| typeof<Generators> |],
        MaxTest = 1000)>]
    do ()

module Fuzz =

    [<Property>]
    let ``Solvable`` solved =
        let solutions = Puzzle.solve solved.Puzzle
        Seq.contains solved.Solution solutions
