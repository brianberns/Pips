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

    /// Constraint display string.
    let private getConstraintString region =
        match region.Type with
            | RegionType.Any          -> ""
            | RegionType.Equal        -> "="
            | RegionType.Unequal      -> "≠"
            | RegionType.SumLess n    -> $"<{n}"
            | RegionType.SumGreater n -> $">{n}"
            | RegionType.SumExact n   -> $"{n}"

    /// Relative darkness of a region's cells. Regions with
    /// tighter constraints are shaded more heavily.
    let private getShadeLevel region =
        match region.Type with
            | RegionType.Any          -> 0
            | RegionType.Equal        -> 1
            | RegionType.Unequal      -> 2
            | RegionType.SumLess _    -> 3
            | RegionType.SumGreater _ -> 4
            | RegionType.SumExact _   -> 5

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
    let private renderCell regionMap region constraintStr cell =
        let toRight = { cell with Column = cell.Column + 1 }
        let below = { cell with Row = cell.Row + 1 }
        [
            Html.div [
                prop.key $"{cell}"
                prop.className "cell"
                prop.style [
                    style.gridRowStart (cell.Row + 1)
                    style.gridColumnStart (cell.Column + 1)
                    style.custom ("--shade", $"{getShadeLevel region}")
                ]
                prop.children [
                    if constraintStr <> "" then
                        Html.span [
                            prop.className "constraint"
                            prop.text (constraintStr : string)
                        ]
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

    /// Renders the cells of the given region. The region's
    /// constraint is displayed in its last cell.
    let render regionMap region =
        let constraintCell = Seq.max region.Cells
        [
            for cell in region.Cells do
                let constraintStr =
                    if cell = constraintCell then
                        getConstraintString region
                    else ""
                yield! renderCell regionMap region constraintStr cell
        ]
