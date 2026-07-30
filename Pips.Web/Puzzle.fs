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

    /// Number of hues available to color regions. Kept low so
    /// that consecutive hues are far enough apart to tell apart
    /// at a glance -- tested empirically down to 5 before a
    /// region ever runs out of hues distinct from its neighbors.
    let private numHues = 6

    /// Hue of the given color class, in degrees.
    let private getHue iColor =
        360.0 * float iColor / float numHues

    /// Cells that share at least a corner with the given cell.
    /// Two regions can meet at a single lattice point without
    /// ever sharing an edge, and a badge sits on exactly that
    /// point, so corner-touching regions need distinct colors
    /// just as much as edge-adjacent ones do.
    let private getTouching cell =
        [|
            for dRow in -1 .. 1 do
                for dCol in -1 .. 1 do
                    if dRow <> 0 || dCol <> 0 then
                        { cell with
                            Row = cell.Row + dRow
                            Column = cell.Column + dCol }
        |]

    /// Chooses a color for every region that has a constraint to
    /// show, so that no two such regions meeting at a shared
    /// edge or corner are colored alike. Regions are indexed by
    /// their position in the array.
    ///
    /// Regions are colored greedily, most crowded first. A
    /// region can have far more corner neighbors than a planar
    /// map would otherwise suggest, so a region with many of
    /// them can still exhaust the palette, in which case two
    /// neighbors have to share.
    let private assignColors (regions : Region[]) =

            // an unconstrained region is always gray, so it
            // neither takes a color nor rules one out for its
            // neighbors
        let colorable =
            [|
                for iRegion = 0 to regions.Length - 1 do
                    if regions[iRegion].Type <> RegionType.Any then
                        yield iRegion
            |]

            // which region does each colorable cell belong to?
        let cellRegions =
            Map [
                for iRegion in colorable do
                    for cell in regions[iRegion].Cells do
                        yield cell, iRegion
            ]

            // which regions does each region touch?
        let neighbors =
            Map [
                for iRegion in colorable do
                    yield iRegion, set [
                        for cell in regions[iRegion].Cells do
                            for adj in getTouching cell do
                                match Map.tryFind adj cellRegions with
                                    | Some iOther when iOther <> iRegion ->
                                        yield iOther
                                    | _ -> ()
                    ]
            ]

            // the most crowded regions get first pick
        let order =
            colorable
                |> Array.sortByDescending (fun iRegion ->
                    neighbors[iRegion].Count)

        (Map.empty, order)
            ||> Array.fold (fun colors iRegion ->
                let taken =
                    neighbors[iRegion]
                        |> Seq.choose (fun iOther ->
                            Map.tryFind iOther colors)
                        |> set
                let iColor =
                    Seq.init numHues id
                        |> Seq.tryFind (taken.Contains >> not)
                        |> Option.defaultValue 0
                Map.add iRegion iColor colors)

    /// Renders the given puzzle's board. A solution is laid over
    /// the board rather than replacing it, so that the regions
    /// and their constraints remain visible underneath.
    let renderBoard puzzle solutionOpt =
        let regionMap = getRegionMap puzzle
        let colors = assignColors puzzle.Regions

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
                        (Map.tryFind iRegion colors |> Option.map getHue)
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
                for iRegion = 0 to puzzle.Regions.Length - 1 do
                    let region = puzzle.Regions[iRegion]
                    match Region.tryGetConstraint region with
                        | Some text ->
                            Region.renderBadge
                                (Region.getBadgeAnchor region)
                                (getHue colors[iRegion])
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
