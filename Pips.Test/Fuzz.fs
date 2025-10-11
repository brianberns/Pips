namespace Pips.Fuzz

open FsCheck.FSharp
open FsCheck.Xunit

open Pips

module Board =

    let gen =
        gen {
            let board = Board.create 10 10
            return board
        }
        
    let arb = Arb.fromGen gen

module Puzzle =

    let gen =
        gen {
            let! board = Board.gen
            let puzzle =
                {
                    UnplacedDominoes = Set.empty
                    Regions = Array.empty
                    Board = board
                }
            return puzzle
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

