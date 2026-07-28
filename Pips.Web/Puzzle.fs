namespace Pips.Web

open Feliz

open Pips

module Puzzle =

    /// Maps each of the given puzzle's cells to its region.
    let private getRegionMap puzzle =
        Map [
            for region in puzzle.Regions do
                for cell in region.Cells do
                    yield cell, region
        ]

    /// Number of textures available to fill regions.
    let private numTextures = 5

    /// Chooses a texture for each of the given regions that has
    /// a constraint to show, so that no two such regions sharing
    /// a border are filled with the same one. Regions are indexed
    /// by their position in the array.
    ///
    /// Regions are filled greedily, most crowded first. A map of
    /// regions is planar, so this rarely calls for more than
    /// four textures, but a region with many neighbors can still
    /// exhaust them, in which case two neighbors have to share.
    let private assignTextures (regions : Region[]) =

            // a region that doesn't constrain its cells is left
            // blank, so it neither takes a texture nor rules one
            // out for its neighbors
        let textured =
            [|
                for iRegion = 0 to regions.Length - 1 do
                    let region = regions[iRegion]
                    if (Region.tryGetConstraint region).IsSome then
                        yield iRegion
            |]

            // which region does each textured cell belong to?
        let cellRegions =
            Map [
                for iRegion in textured do
                    for cell in regions[iRegion].Cells do
                        yield cell, iRegion
            ]

            // which regions does each region touch?
        let neighbors =
            Map [
                for iRegion in textured do
                    yield iRegion, set [
                        for cell in regions[iRegion].Cells do
                            for adj in Cell.getAdjacent cell do
                                match Map.tryFind adj cellRegions with
                                    | Some iOther when iOther <> iRegion ->
                                        yield iOther
                                    | _ -> ()
                    ]
            ]

            // the most crowded regions get first pick
        let order =
            textured
                |> Array.sortByDescending (fun iRegion ->
                    neighbors[iRegion].Count)

        (Map.empty, order)
            ||> Array.fold (fun textures iRegion ->
                let taken =
                    neighbors[iRegion]
                        |> Seq.choose (fun iOther ->
                            Map.tryFind iOther textures)
                        |> set
                let iTexture =
                    Seq.init numTextures id
                        |> Seq.tryFind (taken.Contains >> not)
                        |> Option.defaultValue 0
                Map.add iRegion iTexture textures)

    /// Renders the given puzzle's board. A solution is laid over
    /// the board rather than replacing it, so that the regions
    /// and their constraints remain visible underneath.
    let renderBoard puzzle solutionOpt =
        let regionMap = getRegionMap puzzle
        let textures = assignTextures puzzle.Regions

        Html.div [
            prop.className "board"
            prop.style [
                style.custom ("--rows", $"{puzzle.Board.NumRows}")
                style.custom ("--cols", $"{puzzle.Board.NumColumns}")
            ]
            prop.children [

                    // cells and boundaries of each region
                for iRegion = 0 to puzzle.Regions.Length - 1 do
                    yield! Region.render
                        regionMap
                        (Map.tryFind iRegion textures)
                        puzzle.Regions[iRegion]

                    // dominoes of the solution, if any
                match solutionOpt with
                    | Some (solution : Puzzle) ->
                        for (domino, edge) in solution.Board.DominoPlaces do
                            Domino.renderPlaced domino edge
                    | None -> ()

                    // constraints, which sit above the dominoes.
                    // Regions that don't constrain their cells
                    // have nothing to say, and so have no badge.
                for region in puzzle.Regions do
                    match Region.tryGetConstraint region with
                        | Some text ->
                            Region.renderBadge
                                (Region.getBadgeAnchor region)
                                text
                        | None -> ()
            ]
        ]

    /// Renders the given puzzle's unplaced dominoes.
    let renderUnplacedDominoes puzzle =
        Html.div [
            prop.className "tray"
            prop.children [
                for domino in puzzle.UnplacedDominoes do
                    Domino.renderUnplaced domino
            ]
        ]
