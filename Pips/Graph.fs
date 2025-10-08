namespace Pips

/// An edge connects a pair of adjacent cells and can be
/// covered by a domino.
type Edge = Cell * Cell

/// A graph of connected cells.
type Graph = Map<Cell, Set<Cell> (*adjacent cells*)>

module Graph =

    /// Creates a graph from a set of cells.
    let create (cells : Set<_>) : Graph =
        Map [
            for cell in cells do
                let neighbors =
                    Cell.getAdjacent cell
                        |> Seq.where cells.Contains
                        |> set
                cell, neighbors
        ]

    /// Removes a cell from a graph.
    let private remove cell (graph : Graph) =
        match Map.tryFind cell graph with
            | None -> graph   // cell not in graph
            | Some neighbors ->
                let graph = Map.remove cell graph
                neighbors
                    |> Set.fold (fun g neighbor ->
                        assert(g[neighbor].Contains(cell))
                        Map.change
                            neighbor
                            (Option.map (Set.remove cell))
                            g) graph
    
    /// Finds a connected component starting from a cell
    /// within a graph.
    let private getConnectedComponent start (graph : Graph) =

        let rec loop queue (visited : Set<_>) =
            match queue with
                | [] -> visited
                | cell :: rest ->
                    if visited.Contains(cell) then
                        loop rest visited
                    else
                        let visited = visited.Add(cell)
                        let neighbors =
                            graph[cell]
                                |> Seq.where (
                                    visited.Contains >> not)
                                |> Seq.toList
                        let queue = neighbors @ rest
                        loop queue visited

        loop [start] Set.empty
    
    /// Checks if an edge can be part of a perfect matching.
    /// An edge is impossible if removing it creates a component
    /// with an odd cell count.
    let isEdgePossible cellA cellB (graph : Graph) =
    
        // Check all connected components in the remaining graph
        let rec checkComponents (graph : Graph) =
            if Map.isEmpty graph then
                true   // success: all components have an even cell count
            else
                    // find next component
                let start, _ = Map.toSeq graph |> Seq.head
                let comp = getConnectedComponent start graph

                    // if component is tileable, remove it from graph and check the next component
                if Set.count comp % 2 = 0 then
                    graph
                        |> Set.foldBack remove comp
                        |> checkComponents
                else
                    false   // odd cell count means component is not tileable

            // remove the two cells covered by this edge and check the resulting components
        if Cell.adjacent cellA cellB then
            graph
                |> remove cellA
                |> remove cellB
                |> checkComponents
        else false
