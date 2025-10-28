namespace Pips

module Backtrack =

    /// Gets all possible tiling trees for the given puzzle.
    let private getAllTilingTrees =
        Puzzle.getAllTilings >> TilingTree.ofTilings

    /// Finds all solutions for the given puzzle by back-
    /// tracking.
    let solve puzzle =

        /// Finds all solutions to the given puzzle, guided
        /// by the given possible tiling trees.
        let rec tile tilingTrees puzzle =
            seq {
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
                            if not (Domino.isDouble domino) then
                                let edge = Edge.reverse edge
                                yield! loop childTrees domino edge puzzle
            }

        /// Tries to place the given domino in the given location
        /// and then continue to look for solutions using the given
        /// child tiling trees.
        and loop tilingTrees domino edge puzzle =
            match Puzzle.tryPlace domino edge puzzle with
                | Some puzzle ->
                    tile tilingTrees puzzle
                | None -> Seq.empty

            // solve the puzzle using possible tilings
        let tilingTrees = getAllTilingTrees puzzle
        tile tilingTrees puzzle

    /// Finds an arbitrary solution for the given puzzle by
    /// backtracking, if at least one exists.
    let trySolve puzzle =
        Seq.tryHead (solve puzzle)
