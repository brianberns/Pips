namespace Pips

/// An edge connects a pair of adjacent cells and can be
/// covered by a domino.
type Edge = Cell * Cell

/// A tiling in the form of a tree. Each node represents a
/// single edge with child tilings that cover the remaining
/// edges.
type Tiling = Node of Edge * Tiling[]

module Tiling =
    
    /// Gets all tilings (i.e. "perfect matchings") for a
    /// set of cells.
    let rec getAll (cells : Set<_>) =
        if cells.IsEmpty then Array.empty
        else
                // pick an arbitrary cell
            let cell = Seq.head cells
            
                // try all (normalized) edges that include this cell
            Cell.getAdjacent cell
                |> Seq.where (fun adj ->
                    cells.Contains(adj) && cell < adj)
                |> Seq.map (fun adj -> 

                        // remove this edge from further consideration
                    let cells = cells.Remove(cell).Remove(adj)

                        // get child tilings
                    let children = getAll cells
                    Node ((cell, adj), children))

                |> Seq.toArray
