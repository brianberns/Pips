namespace Pips

module Backtrack =

    /// Gets all possible tiling trees for the given puzzle.
    let private getAllTilingTrees =
        Puzzle.getAllTilings >> TilingTree.ofTilings

    /// Finds all solutions for the given puzzle by back-
    /// tracking. This can take a while!
    let solve puzzle =

        /// Finds all solutions to the given puzzle, guided
        /// by the given possible tiling trees.
        let rec tile tilingTrees puzzle =
            [|
                if Puzzle.isValid puzzle then

                        // all dominoes have been placed successfully?
                    if puzzle.UnplacedDominoes.IsEmpty then
                        assert(Puzzle.isSolved puzzle)
                        puzzle
                    else
                            // try each possible tiling
                        for tilingTree in tilingTrees do

                                // get edge to cover in this tiling
                            let (Node (edge, childTrees)) = tilingTree

                                // try each domino on that edge
                            for domino in puzzle.UnplacedDominoes do
                                yield! loop childTrees domino edge puzzle
                                if domino.Left <> domino.Right then
                                    let edge = Edge.reverse edge
                                    yield! loop childTrees domino edge puzzle
            |]

        /// Places the given domino in the given location and
        /// then continues to look for solutions using the given
        /// child tiling trees.
        and loop tilingTrees domino edge puzzle =
            Puzzle.place domino edge puzzle
                |> tile tilingTrees

            // solve the puzzle using possible tilings
        let tilingTrees = getAllTilingTrees puzzle
        let solutions = tile tilingTrees puzzle
        assert(
            (Array.distinct solutions).Length = solutions.Length)
        solutions

    /// Finds an arbitrary solution for the given puzzle by
    /// backtracking, if at least one exists. This can take a
    /// while!
    let trySolve puzzle =

        /// Tries to find a solution to the given puzzle, guided
        /// by the given possible tilings.
        let rec tile tilingTrees puzzle =
            tryPick {
                if Puzzle.isValid puzzle then

                        // all dominoes have been placed successfully?
                    if puzzle.UnplacedDominoes.IsEmpty then
                        assert(Puzzle.isSolved puzzle)
                        puzzle
                    else
                            // try each possible tiling
                        for tilingTree in tilingTrees do

                                // get edge to cover in this tiling
                            let (Node (edge, childTrees)) = tilingTree

                                // try each domino on that edge
                            for domino in puzzle.UnplacedDominoes do
                                yield! loop childTrees domino edge puzzle
                                if domino.Left <> domino.Right then
                                    let edge = Edge.reverse edge
                                    yield! loop childTrees domino edge puzzle
            }

        /// Places the given domino in the given location and
        /// then continues to look for solutions using the given
        /// child tilings.
        and loop tilings domino edge puzzle =
            Puzzle.place domino edge puzzle
                |> tile tilings

            // solve the puzzle using possible tilings
        let tilingTrees = getAllTilingTrees puzzle
        tile tilingTrees puzzle
