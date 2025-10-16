namespace Pips

/// A tiling is a set of edges.
type Tiling = Set<Edge>

module Tiling =

    /// Gets all possible cells adjacent to the given cell that
    /// create a normalized edge. Some of these cells might not
    /// actually exist, though.
    let private getAdjacentNormalized cell =
        [|
            { cell with Row = cell.Row + 1 }
            { cell with Column = cell.Column + 1 }
        |]
    
    /// Gets all tilings for the given set of cells.
    let getAll cells =
        
        let rec loop (unvisited : Set<_>) (tiling : Tiling) =
            [|
                if unvisited.IsEmpty then
                    tiling   // yield complete tiling on success
                else
                    let cell = Seq.head unvisited
                    let unvisited = unvisited.Remove(cell)
                    for adj in getAdjacentNormalized cell do
                        if unvisited.Contains(adj) then
                            let unvisited = unvisited.Remove(adj)
                            yield! loop unvisited (tiling.Add(cell, adj))
            |]

        let tilings = loop cells Set.empty
        assert(
            (Array.distinct tilings).Length = tilings.Length)
        tilings

/// Multiple tilings in the form of a tree. Each node
/// represents a single edge with child nodes that cover
/// the remaining edges.
type TilingTree = Node of Edge * TilingTree[]

module TilingTree =
    
    /// Builds a forest from the given tilings.
    let ofTilings tilings =

        let rec loop (tilings : Tiling[]) =
            if tilings.Length = 0 then
                Array.empty   // all done
            else
                assert(
                    tilings
                        |> Array.distinctBy _.Count
                        |> Array.length = 1)

                    // partition tilings using an arbitrary edge
                let edge = Seq.head tilings[0]
                let present, absent =
                    tilings
                        |> Array.partition _.Contains(edge)

                    // remove edge from tilings
                let removed =
                    [|
                        for tiling in present do
                            let tiling = tiling.Remove(edge)
                            if not tiling.IsEmpty then
                                tiling
                    |]

                [|
                        // node for this edge
                    Node (edge, loop removed)

                        // nodes for tilings that don't contain this edge
                    yield! loop absent
                |]

        loop tilings
