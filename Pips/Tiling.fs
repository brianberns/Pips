namespace Pips

/// An edge connects a pair of adjacent cells and can be
/// covered by a domino.
type Edge = Cell * Cell

/// A tiling in the form of a tree. Each node represents a
/// single edge with child tilings that cover the remaining
/// edges.
type Tiling = Node of Edge * Tiling[]

module Tiling =
    
    /// Finds all perfect matchings for a set of cells.
    let findPerfectMatchings cells =

        let rec loop (cells : Set<_>) =
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

                            // perfect matching is possible?
                        let children =
                            if cells.IsEmpty then Array.empty
                            else loop cells
                        Node ((cell, adj), children))

                    |> Seq.toArray

        loop (set cells)
