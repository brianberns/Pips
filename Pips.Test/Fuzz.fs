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

    /// All dominoes from 0-0 to 6-6.
    let allDominoes =
        let range = [| PipCount.minValue .. PipCount.maxValue |]
        Array.allPairs range range
            |> Array.map (uncurry Domino.create)

    /// Board we'll create puzzles on.
    let emptyBoard = Board.create 8 8

    /// All normalized edges in the empty board.
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

    /// Places dominoes on empty edges in the given puzzle.
    let place puzzle =

        /// Places dominoes one at a time.
        let rec loop emptyEdges puzzle =
            gen {
                    // all dominoes have been placed?
                if puzzle.UnplacedDominoes.IsEmpty then
                    return puzzle
                else
                        // pick an arbitrary domino to place
                    let! domino = Gen.elements puzzle.UnplacedDominoes

                        // pick an arbitrary edge to place it on
                    if Array.isEmpty emptyEdges then
                        failwith "Board too small"
                    let! edge : Edge = Gen.elements emptyEdges

                        // pick an arbitrary orientation for the domino
                    let! flag = Gen.elements [ true; false ]
                    let edge =
                        if domino.Left = domino.Right || flag then edge   // avoid creating a denormalized placement
                        else Edge.reverse edge

                        // place the domino
                    let puzzle =
                        Puzzle.place domino edge puzzle

                        // remove any edges that are now at least partially covered by this domino
                    let emptyEdges =
                        emptyEdges
                            |> Array.where (fun (cellA, cellB) ->
                                not (Edge.contains cellA edge)
                                    && not (Edge.contains cellB edge))

                        // continue placing dominoes
                    return! loop emptyEdges puzzle
            }

        loop allEmptyEdges puzzle

    let getContigousCells cell (cells : Set<_>) board =

        let rec visit cell (visited : Set<_>) =
            if visited.Contains(cell) then
                visited
            else
                let visited = visited.Add(cell)
                let neighbors =
                    Board.getAdjacent cell board
                        |> Seq.where cells.Contains
                (neighbors, visited)
                    ||> Seq.foldBack visit

        visit cell Set.empty

    let createRegion cells board =
        gen {
            let! cell = Gen.elements cells
            let contiguous =
                getContigousCells cell cells board
            let region =
                {
                    Cells = Seq.toArray contiguous
                    Type = RegionType.Any
                }
            return region, cells - contiguous
        }

    let createRegions cells board =

        let rec loop (cells : Set<_>) regions =
            gen {
                if cells.IsEmpty then
                    return regions
                else
                    let! region, cells =
                        createRegion cells board
                    return! loop cells (region :: regions)
            }

        loop (set cells) List.empty
            |> Gen.map Seq.toArray

    let gen =
        gen {
                // pick an arbitrary subset of dominoes (w/ no duplicates)
            let! dominoes =
                gen {
                    let! n = Gen.choose(0, min 12 allDominoes.Length)   // keep to a reasonable number of dominoes
                    let! shuffled = Gen.shuffle allDominoes
                    return Seq.truncate n shuffled
                }

                // place the dominoes on an empty puzzle to create a solution
            let puzzle =
                {
                    UnplacedDominoes = set dominoes
                    Regions = Array.empty
                    Board = emptyBoard
                }
            let! solution = place puzzle

                // gather all covered cells in the solution.
            let cells =
                solution.Board.DominoPlaces
                    |> Seq.collect (fun (_, (cellA, cellB)) ->
                        [ cellA; cellB ])
                    |> Seq.toArray

            let! regions = createRegions cells puzzle.Board
            let puzzle =
                { puzzle with Regions = regions }
            let solution =
                { solution with Regions = regions }
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
        MaxTest = 10)>]
    do ()

module Fuzz =

    [<Property>]
    let ``Solvable`` solved =
        let solutions = Puzzle.solve solved.Puzzle
        Seq.contains solved.Solution solutions
