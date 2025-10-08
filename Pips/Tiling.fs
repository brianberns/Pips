namespace Pips

/// A tiling in the form of a tree. Each node represents a
/// single edge with child tilings that cover the remaining
/// edges.
type Tiling = Node of Cell * Cell * Tiling[]

module Tiling =
    
    /// Gets all tilings (i.e. "perfect matchings") for a
    /// set of cells.
    let rec getAll (cells : Set<_>) =
        if cells.IsEmpty then
            Array.empty   // all done: perfect matching found
        else
                // pick an arbitrary cell
            let cell = Seq.head cells
            
                // try all edges that include this cell
            Cell.getAdjacent cell
                |> Array.choose (fun adj ->
                    if cells.Contains(adj) && cell < adj then   // normalize edges to avoid redundancy

                            // remove this edge from further consideration
                        let cells = cells.Remove(cell).Remove(adj)

                            // get child tilings
                        Some (Node (cell, adj, getAll cells))

                    else None)
