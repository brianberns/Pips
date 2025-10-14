namespace Pips

/// Multiple tilings in the form of a tree. Each node
/// represents a single edge with child nodes that cover
/// the remaining edges.
type TilingTree = Node of Edge * TilingTree[]

module TilingTree =
    
    /// Gets all tiling trees (i.e. "perfect matchings")
    /// for a set of cells.
    let getAll cells =

        let rec loop (cells : Set<_>) =
            if cells.IsEmpty then
                Some Array.empty   // all done: perfect matching found
            else
                    // pick an arbitrary cell
                let cell = Seq.head cells
            
                    // try all edges that include this cell
                let trees =
                    Cell.getAdjacent cell
                        |> Array.choose (fun adj ->
                            if cells.Contains(adj) && cell < adj then   // normalize edges to avoid redundancy

                                    // remove this edge from further consideration
                                let cells =
                                    cells.Remove(cell).Remove(adj)

                                    // get child nodes
                                loop cells
                                    |> Option.map (fun trees ->
                                        Node ((cell, adj), trees))

                            else None)

                    // tiled this cell successfully?
                if trees.Length = 0 then None
                else Some trees

        loop cells
            |> Option.defaultValue Array.empty

    /// One tiling in the form of a list of edges.
    type private Tiling = List<Edge>

    /// Gets all tilings in the given tiling tree.
    let rec private getAllTilings
        (Node (edge, children)) : List<Tiling> =
        [
            if children.Length = 0 then
                [ edge ]
            else
                for child in children do
                    for path in getAllTilings child do
                        edge :: path
        ]

    /// Gets edges that are forced to appear in all of the
    /// given tiling trees.
    let getForcedEdges tilingTrees =
        tilingTrees
            |> Seq.collect getAllTilings
            |> Seq.map set
            |> Set.intersectMany
