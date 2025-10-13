namespace Pips

/// A tiling in the form of a tree. Each node represents a
/// single edge with child tilings that cover the remaining
/// edges.
type Tiling = Node of Edge * Tiling[]

module Tiling =
    
    /// Gets all tilings (i.e. "perfect matchings") for a
    /// set of cells.
    let getAll cells =

        let rec loop (cells : Set<_>) : Option<_[]> =
            if cells.IsEmpty then
                Some Array.empty   // all done: perfect matching found
            else
                    // pick an arbitrary cell
                let cell = Seq.head cells
            
                    // try all edges that include this cell
                let tilings =
                    Cell.getAdjacent cell
                        |> Array.choose (fun adj ->
                            if cells.Contains(adj) && cell < adj then   // normalize edges to avoid redundancy

                                    // remove this edge from further consideration
                                let cells = cells.Remove(cell).Remove(adj)

                                    // get child tilings
                                loop cells
                                    |> Option.map (fun tilings ->
                                        Node ((cell, adj), tilings))

                            else None)

                    // tiled this cell successfully?
                if tilings.Length = 0 then None
                else Some tilings

        loop cells
            |> Option.defaultValue Array.empty

    type private Path = List<Edge>

    let rec private getAllPaths (Node (edge, children)) =
        [
            if children.Length = 0 then
                [ edge ]
            else
                for child in children do
                    for path in getAllPaths child do
                        edge :: path : Path
        ]

    let getForced tilings =
        tilings
            |> Seq.collect getAllPaths
            |> Seq.map set
            |> Set.intersectMany
