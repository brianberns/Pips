namespace Pips

/// A Pips puzzle in some state of being solved.
type Puzzle =
    {
        /// Available dominoes that have not yet been placed
        /// on the board.
        UnplacedDominoes : Set<Domino>   // assume no duplicates

        /// Regions of cells that impose constraints on the
        /// dominoes placed there.
        Regions : Region[]

        /// A board of cells, some of which may be covered
        /// with dominoes.
        Board : Board
    }

module Puzzle =

    /// Creates a puzzle in its initial state, where no dominoes
    /// have yet been placed on the board.
    let create dominoes regions =
        assert(
            Seq.distinct dominoes |> Seq.length
                = Seq.length dominoes)
        let cells =
            regions
                |> Array.collect _.Cells
        let maxRow =
            cells
                |> Seq.map _.Row
                |> Seq.max
        let maxColumn =
            cells
                |> Seq.map _.Column
                |> Seq.max
        {
            UnplacedDominoes = set dominoes
            Regions = regions
            Board = Board.create (maxRow + 1) (maxColumn + 1)
        }

    /// Is the given puzzle in a valid state?
    let isValid puzzle =
        puzzle.Regions
            |> Array.forall (
                Region.isValid puzzle.Board)

    /// Is the given puzzle completely solved? (Note that
    /// a solved puzzle is in a valid state, but a valid
    /// puzzle might not be solved.)
    let isSolved puzzle =
        puzzle.Regions
            |> Array.forall (
                Region.isSolved puzzle.Board)

    /// Is the given cell on the given puzzle's board not
    /// covered by a domino?
    let isEmpty cell puzzle =
        Board.isEmpty puzzle.Board cell

    /// Places the given domino in the given location in
    /// the given puzzle.
    let place domino edge puzzle =
        {
            puzzle with
                UnplacedDominoes =
                    puzzle.UnplacedDominoes.Remove(domino)
                Board =
                    Board.place domino edge puzzle.Board
        }

    /// Gets all possible tilings for the given puzzle.
    let private getAllTilings puzzle =
        puzzle.Regions
            |> Seq.collect _.Cells
            |> Seq.where (flip isEmpty puzzle)
            |> set
            |> Tiling.getAll

    /// Splits the given region into single-cell regions
    /// that all have the given target.
    let private splitRegion target region =
        [|
            for cell in region.Cells do
                {
                    Cells = [| cell |]
                    Type = RegionType.SumExact target
                }
        |]

    /// Infers smaller, more specific regions from the given
    /// region, if possible.
    let private inferRegion region =
        let minSum =
            PipCount.minValue * region.Cells.Length
        let maxSum =
            PipCount.maxValue * region.Cells.Length
        match region.Type with
            | RegionType.SumExact n when n = minSum ->
                splitRegion PipCount.minValue region
            | RegionType.SumExact n when n = maxSum ->
                splitRegion PipCount.maxValue region
            | _ -> [| region |]

    /// Infers domino placements in the given puzzle, if
    /// possible.
    let private infer puzzle =

            // infer regions
        let puzzle =
            let regions =
                puzzle.Regions
                    |> Array.collect inferRegion
            { puzzle with Regions = regions }

            // get map of forced values (single-cell sum regions)
        let valueMap =
            puzzle.Regions
                |> Seq.choose (fun region ->
                    tryPick {
                        let! cell = Seq.tryExactlyOne region.Cells
                        match region.Type with
                            | RegionType.SumExact n ->
                                return cell, n
                            | _ -> ()
                    })
                |> Map

            // match each forced edge to a domino
        let tilings = getAllTilings puzzle
        let pairs =
            tilings
                |> Set.intersectMany
                |> Seq.choose (fun ((cellA, cellB) as edge) ->
                    let valueOptA = Map.tryFind cellA valueMap
                    let valueOptB = Map.tryFind cellB valueMap
                    match valueOptA, valueOptB with
                        | Some valueA, Some valueB ->
                            let domino = Domino.create valueA valueB
                            if puzzle.UnplacedDominoes.Contains(domino) then
                                Some (domino, edge)
                            else
                                let domino = Domino.create valueB valueA
                                assert(puzzle.UnplacedDominoes.Contains(domino))
                                Some (domino, Edge.reverse edge)
                        | _ -> None)
                |> Seq.toArray

            // place each forced domino
        let puzzle =
            Seq.fold (fun puzzle (domino, edge) ->
                place domino edge puzzle) puzzle pairs

            // recompute tilings (to-do: can this be done faster?)
        let tilings =
            if pairs.Length = 0 then tilings
            else getAllTilings puzzle

        puzzle, tilings

    /// Maps cells to edges that contain them.
    let private createEdgeMap edges =
        seq {
            for ((cellA, cellB) as edge) : Edge in edges do
                yield cellA, edge
                yield cellB, edge
        }
            |> Seq.groupBy fst
            |> Seq.map (fun (cell, group) ->
                let edges =
                    group
                        |> Seq.map snd
                        |> set
                cell, edges)
            |> Map

    /// Finds all solutions for the given puzzle by backtracking.
    /// This can take a while!
    let private backtrack edges puzzle =

        /// Finds all solutions to the given puzzle, guided
        /// by the given possible edges.
        let rec tile (edges : Set<_>) (edgeMap : Map<_, _>) puzzle =
            [
                if isValid puzzle then

                        // all dominoes have been placed successfully?
                    if puzzle.UnplacedDominoes.IsEmpty then
                        assert(isSolved puzzle)
                        puzzle
                    else
                            // get edge to cover in this tiling
                        let (cellA, cellB) as edge = Seq.head edges
                        let edges = edges - edgeMap[cellA] - edgeMap[cellB]
                        let edgeMap = edgeMap.Remove(cellA).Remove(cellB)

                            // try each domino on that edge
                        for domino in puzzle.UnplacedDominoes do
                            yield! loop domino edge edges edgeMap puzzle
                            if domino.Left <> domino.Right then
                                let edge = Edge.reverse edge
                                yield! loop domino edge edges edgeMap puzzle
            ]

        /// Places the given domino in the given location and
        /// then continues to look for solutions using the given
        /// edges.
        and loop domino edge edges edgeMap puzzle =
            place domino edge puzzle
                |> tile edges edgeMap
                    
        let edgeMap = createEdgeMap edges
        tile edges edgeMap puzzle

    /// Finds an arbitrary solution for the given puzzle by
    /// backtracking, if at least one exists. This can take a
    /// while!
    let private tryBacktrack edges puzzle =

            // map cells to edges that contain them
        let edgeMap = createEdgeMap edges

        /// Tries to find a solution to the given puzzle, guided
        /// by the given possible edges.
        let rec tile (edges : Set<_>) (edgeMap : Map<_, _>) puzzle =
            tryPick{
                if isValid puzzle then

                        // all dominoes have been placed successfully?
                    if puzzle.UnplacedDominoes.IsEmpty then
                        assert(isSolved puzzle)
                        puzzle
                    else
                            // get edge to cover in this tiling
                        let (cellA, cellB) as edge = Seq.head edges
                        let edges = edges - edgeMap[cellA] - edgeMap[cellB]
                        let edgeMap = edgeMap.Remove(cellA).Remove(cellB)

                            // try each domino on that edge
                        for domino in puzzle.UnplacedDominoes do
                            yield! loop domino edge edges edgeMap puzzle
                            if domino.Left <> domino.Right then
                                let edge = Edge.reverse edge
                                yield! loop domino edge edges edgeMap puzzle
            }

        /// Places the given domino in the given location and
        /// then continues to look for solutions using the given
        /// edges.
        and loop domino edge edges edgeMap puzzle =
            place domino edge puzzle
                |> tile edges edgeMap
                    
        let edgeMap = createEdgeMap edges
        tile edges edgeMap puzzle

    /// Finds all solutions for the given puzzle.
    let solve puzzle =
        let puzzle', tilings = infer puzzle
        let edges = Set.unionMany tilings
        backtrack edges puzzle'
            |> List.map (fun solution ->
                { solution with Regions = puzzle.Regions })

    /// Finds an arbitrary solution for the given puzzle,
    /// if at least one exists.
    let trySolve puzzle =
        let puzzle', tilings = infer puzzle
        let edges = Set.unionMany tilings
        tryBacktrack edges puzzle'
            |> Option.map (fun solution ->
                { solution with Regions = puzzle.Regions })
