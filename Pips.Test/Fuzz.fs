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

    module Gen =

        /// Selects the given number of items from the given
        /// sequence arbitrarily.
        let truncate n xs =
            gen {
                let! shuffled = Gen.shuffle xs
                return Array.truncate n shuffled
            }

    /// All dominoes from 0-0 to 6-6.
    let allDominoes =
        let range = [| PipCount.minValue .. PipCount.maxValue |]
        Array.allPairs range range
            |> Array.map (uncurry Domino.create)

    /// Height and width of board.
    let boardSize = 6

    /// Minimum number of dominoes to place.
    let minNumDominoes = 7

    /// Maximum number of dominoes to place.
    let maxNumDominoes = 9

    /// Board we'll create puzzles on.
    let emptyBoard = Board.create boardSize boardSize

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

    let getRegionPipValues (cells : _[]) (board : Board) =
        let pipCounts =
            Array.map board.Item cells
        assert(
            Array.forall (fun pipCount ->
                pipCount <> Board.emptyCell) pipCounts)
        pipCounts

    let tryCreateUnconstrainedRegion (cells : _[]) _board =
        gen {
            if cells.Length = 1 then
                return Some {
                    Cells = cells
                    Type = RegionType.Any
                }
            else return None
        }

    let tryCreateEqualRegion (cells : _[]) board =
        gen {
            if cells.Length > 1 then
                let pipCounts =
                    getRegionPipValues cells board
                        |> Array.distinct
                if pipCounts.Length = 1 then
                    return Some {
                        Cells = cells
                        Type = RegionType.Equal
                    }
                else return None
            else return None
        }

    let tryCreateUnequalRegion (cells : _[]) board =
        gen {
            if cells.Length > 1 then
                let pipCounts =
                    getRegionPipValues cells board
                        |> Array.distinct
                if pipCounts.Length = cells.Length then
                    return Some {
                        Cells = cells
                        Type = RegionType.Unequal
                    }
                else return None
            else return None
        }

    let tryCreateSumLessRegion (cells : _[]) board =
        gen {
            if cells.Length <= 2 then
                let pipCounts =
                    getRegionPipValues cells board
                let sum = Array.sum pipCounts
                let max = pipCounts.Length * PipCount.maxValue
                if sum < max then
                    let! target = Gen.choose (sum + 1, max)
                    return Some {
                        Cells = cells
                        Type = RegionType.SumLess target
                    }
                else return None
            else return None
        }

    let tryCreateSumGreaterRegion (cells : _[]) board =
        assert(PipCount.minValue = 0)
        gen {
            if cells.Length <= 2 then
                let pipCounts =
                    getRegionPipValues cells board
                let sum = Array.sum pipCounts
                if sum > 0 then
                    let! target = Gen.choose (0, sum - 1)
                    return Some {
                        Cells = cells
                        Type = RegionType.SumGreater target
                    }
                else return None
            else return None
        }

    let tryCreateSumRegion cells board =
        gen {
            let pipCounts =
                getRegionPipValues cells board
            let sum = Array.sum pipCounts
            return Some {
                Cells = cells
                Type = RegionType.Sum sum
            }
        }

    let regionFactories =
        [|
            tryCreateUnconstrainedRegion
            tryCreateEqualRegion
            tryCreateUnequalRegion
            tryCreateSumLessRegion
            tryCreateSumGreaterRegion
            tryCreateSumRegion
        |]

    let createRegion cells board =
        gen {
            let! cell = Gen.elements cells
            let! contiguous =
                getContigousCells cell cells board
                    |> Gen.truncate 6
            let! regions =
                regionFactories
                    |> Array.map (fun factory ->
                        factory contiguous board)
                    |> Gen.sequenceToArray
                    |> Gen.map (Array.choose id)
            if Array.isEmpty regions then
                failwith "No matching factory"
            let! region = Gen.elements regions
            return region, cells - set contiguous
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
                    let! n =
                        Gen.choose(
                            minNumDominoes,
                            min maxNumDominoes allDominoes.Length)
                    return! Gen.truncate n allDominoes
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

            let! regions = createRegions cells solution.Board
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
        MaxTest = 100)>]
    do ()

module Fuzz =

    [<Property>]
    let ``Can find solution to solvable puzzle`` solved =
        let solutions = Puzzle.solve solved.Puzzle
        Seq.contains solved.Solution solutions
