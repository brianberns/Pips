namespace Pips

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

                    // remove edge from tilings that contain it
                let removed =
                    [|
                        for tiling in present do
                            let tiling = tiling.Remove(edge)
                            if not tiling.IsEmpty then
                                tiling
                    |]
                assert(
                    removed.Length = present.Length
                        || removed.Length = 0)

                [|
                        // node for this edge
                    Node (edge, loop removed)

                        // nodes for tilings that don't contain this edge
                    yield! loop absent
                |]

        loop tilings
