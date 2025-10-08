namespace Pips

/// An edge connects a pair of adjacent cells and can be
/// covered by a domino.
type Edge = Cell * Cell

/// A tiling in the form of a tree. Each node represents a
/// single edge with child tilings that cover the remaining
/// edges.
type Tiling = Node of Edge * List<Tiling>

module Tiling =
    
    /// Finds all connected components in a set of cells.
    let private getConnectedComponents (cells : Set<Cell>) =

        let rec dfs (visited : Set<_>) queue comp =
            match queue with
                | [] -> comp
                | cell :: rest ->
                    if visited.Contains(cell) then
                        dfs visited rest comp
                    else
                        let visited = visited.Add(cell)
                        let neighbors =
                            Cell.getAdjacent cell
                                |> List.where (fun adj ->
                                    cells.Contains(adj)
                                        && not (visited.Contains(adj)))
                        dfs visited (neighbors @ rest) (Set.add cell comp)
        
        let rec findComponents remaining acc =
            if Set.isEmpty remaining then
                acc
            else
                let start = Seq.head remaining
                let comp = dfs Set.empty [start] Set.empty
                findComponents (remaining - comp) (comp :: acc)
        
        findComponents cells []
        
    /// Checks if a set of cells can potentially have a perfect
    /// matching. All connected components must have an even
    /// number of cells.
    let private canHavePerfectMatching (cells : Set<Cell>) =
        if Set.isEmpty cells then true
        else
            getConnectedComponents cells
                |> Seq.forall (fun comp ->
                    comp.Count % 2 = 0)
    
    /// Finds all perfect matchings starting with a given edge.
    let rec private loop (cells : Set<_>) ((cellA, cellB) as edge : Edge) =

            // remove this edge from further consideration
        let cells = cells.Remove(cellA).Remove(cellB)

            // perfect matching is possible?
        if not (canHavePerfectMatching cells) then
            None
        elif Set.isEmpty cells then
            Some (Node (edge, []))   // leaf node
        else
            Some (Node (edge, findAllMatchings cells))
    
    /// Finds all perfect matchings for a set of cells.
    and private findAllMatchings (cells : Set<_>) =
        if cells.IsEmpty then []
        elif not (canHavePerfectMatching cells) then []
        else
                // pick an arbitrary cell
            let cell = Seq.head cells
            
                // try all (normalized) edges that include this cell
            Cell.getAdjacent cell
                |> List.choose (fun adj -> 
                    if cells.Contains(adj) && cell < adj then
                        loop cells (cell, adj)
                    else None)
    
    /// Main entry point.
    let findPerfectMatchings cells =
        findAllMatchings (set cells)
