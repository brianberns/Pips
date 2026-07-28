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
    let private renderCell regionMap iTextureOpt cell =
        let toRight = { cell with Column = cell.Column + 1 }
        let below = { cell with Row = cell.Row + 1 }
        [
            Html.div [
                prop.key $"{cell}"
                prop.classes [
                    "cell"
                    match iTextureOpt with
                        | Some iTexture -> $"texture-{iTexture}"
                        | None -> ()
                ]
                prop.style [
                    style.gridRowStart (cell.Row + 1)
                    style.gridColumnStart (cell.Column + 1)
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
    /// given texture, or left blank if it has none.
    let render regionMap iTextureOpt (region : Region) =
        [
            for cell in region.Cells do
                yield! renderCell regionMap iTextureOpt cell
        ]

    /// How far a badge is drawn into its own region, in cells.
    let private badgePull = 0.175

    /// Determines where the given region's badge goes, in cell
    /// coordinates.
    ///
    /// A badge marks the top left corner of the region's first
    /// cell, reading left to right and top to bottom. Regions
    /// never share a cell, so they never share a first cell,
    /// and so no two badges are ever drawn at the same corner.
    /// Distinct corners are a whole cell apart, which is wider
    /// than a badge, and the pull below shifts every badge
    /// alike, so badges cannot collide.
    ///
    /// The badge is drawn into its region far enough to show
    /// plainly whose it is. A corner is the point furthest from
    /// every pip of a domino covering the cells around it, so
    /// this costs as little as it can: the badge hides the one
    /// corner pip of its own cell, and no other.
    let getBadgeAnchor (region : Region) =
        let cell = Array.min region.Cells
        float cell.Row + badgePull,
        float cell.Column + badgePull

    /// Renders the given constraint as a badge at the given
    /// point on the board.
    let renderBadge (row, col) (constraintStr : string) =
        Html.div [
            prop.key $"{constraintStr}@{row},{col}"
            prop.classes [
                "badge"
                $"badge-{constraintStr.Length}"
            ]
            prop.style [
                style.custom ("--row", $"{row}")
                style.custom ("--col", $"{col}")
            ]
            prop.children [
                Html.span constraintStr
            ]
        ]
