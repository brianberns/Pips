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
    
    /// Gets all tilings (i.e. "perfect matchings") for the given
    /// set of cells.
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
