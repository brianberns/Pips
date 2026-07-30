namespace Pips.Web

open Feliz

open Pips

module Region =

    /// Are the given cells in the same region?
    let private inSameRegion regionMap cellA cellB =
        Map.tryFind cellA regionMap
            = Map.tryFind cellB regionMap

    /// Is the given cell part of any region?
    let private exists regionMap cell =
        Map.containsKey cell regionMap

    /// Does the given cell have a left border?
    let private hasLeftBorder regionMap cell =
        let adj = { cell with Column = cell.Column - 1 }
        not (inSameRegion regionMap cell adj)

    /// Does the given cell have a top border?
    let private hasTopBorder regionMap cell =
        let adj = { cell with Row = cell.Row - 1 }
        not (inSameRegion regionMap cell adj)

    /// Constraint display string, for a region that constrains
    /// its cells at all.
    let tryGetConstraint region =
        match region.Type with
            | RegionType.Any          -> None
            | RegionType.Equal        -> Some "="
            | RegionType.Unequal      -> Some "≠"
            | RegionType.SumLess n    -> Some $"<{n}"
            | RegionType.SumGreater n -> Some $">{n}"
            | RegionType.SumExact n   -> Some $"{n}"

    /// Renders a line along one edge of a cell. Lines are
    /// centered on the grid and overhang each end by half their
    /// width, so that two of them meeting at a corner always
    /// overlap.
    let private renderBorder isOuter isHorizontal row col =
        Html.div [
            prop.key $"""{if isHorizontal then "h" else "v"}{row},{col}"""
            prop.classes [
                "border"
                if isHorizontal then "border-horizontal"
                else "border-vertical"
                if isOuter then "border-outer"
            ]
            prop.style [
                style.custom ("--row", $"{row}")
                style.custom ("--col", $"{col}")
            ]
        ]

    /// Renders the given cell, along with the lines it is
    /// responsible for drawing.
    ///
    /// Each cell draws its own left and top borders, so every
    /// line between two cells is drawn exactly once. Cells on
    /// the right or bottom edge of the board have no neighbor
    /// to draw those lines, so they draw them instead.
    let private renderCell regionMap (hueOpt : float option) cell =
        let toRight = { cell with Column = cell.Column + 1 }
        let below = { cell with Row = cell.Row + 1 }
        [
            Html.div [
                prop.key $"{cell}"
                prop.classes [
                    "cell"
                    if hueOpt.IsNone then "cell-any"
                ]
                prop.style [
                    style.gridRowStart (cell.Row + 1)
                    style.gridColumnStart (cell.Column + 1)
                    style.custom (
                        "--hue", $"{hueOpt |> Option.defaultValue 0.0}")
                ]
            ]

            renderBorder
                (hasLeftBorder regionMap cell) false
                cell.Row cell.Column
            renderBorder
                (hasTopBorder regionMap cell) true
                cell.Row cell.Column

            if not (exists regionMap toRight) then
                renderBorder true false
                    cell.Row (cell.Column + 1)
            if not (exists regionMap below) then
                renderBorder true true
                    (cell.Row + 1) cell.Column
        ]

    /// Renders the cells of the given region, filled with the
    /// given hue, or gray if it has none (an unconstrained
    /// region is always gray, regardless of any color it was
    /// assigned).
    let render regionMap hueOpt (region : Region) =
        [
            for cell in region.Cells do
                yield! renderCell regionMap hueOpt cell
        ]

    /// Determines where the given region's badge goes, in cell
    /// coordinates: the shared corner of the region's first
    /// cell, reading left to right and top to bottom. Regions
    /// never share a cell, so they never share this corner with
    /// another region's badge.
    ///
    /// The badge sits exactly on the grid, rather than pulled
    /// into its own region, because it is filled with that
    /// region's own color: matching color, not position, is
    /// what tells a reader whose badge it is.
    let getBadgeAnchor (region : Region) =
        let cell = Array.min region.Cells
        float cell.Row, float cell.Column

    /// Renders the given constraint as a badge, filled with the
    /// given hue, at the given point on the board.
    let renderBadge (row, col) hue (constraintStr : string) =
        Html.div [
            prop.key $"{constraintStr}@{row},{col}"
            prop.classes [
                "badge"
                $"badge-{constraintStr.Length}"
            ]
            prop.style [
                style.custom ("--row", $"{row}")
                style.custom ("--col", $"{col}")
                style.custom ("--hue", $"{hue}")
            ]
            prop.children [
                Html.span constraintStr
            ]
        ]
