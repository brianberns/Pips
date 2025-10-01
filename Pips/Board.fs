namespace Pips

type Board =
    {
        Dominoes : Set<Domino * Cell * Cell>
        CellMap : Map<Cell, Value>
    }

module Board =

    let empty =
        {
            Dominoes = Set.empty
            CellMap = Map.empty
        }

    let tryGetValue cell board =
        board.CellMap
            |> Map.tryFind cell

    let isEmpty board cell =
        not (board.CellMap.ContainsKey(cell))

    let place domino cellLeft cellRight board =
        assert(Cell.adjacent cellLeft cellRight)
        assert(isEmpty board cellLeft)
        assert(isEmpty board cellRight)
        {
            CellMap =
                board.CellMap
                    |> Map.add cellLeft domino.Left
                    |> Map.add cellRight domino.Right
            Dominoes =
                board.Dominoes
                    |> Set.add (domino, cellLeft, cellRight)
        }
