namespace Pips

type Board =
    {
        // Dominoes : Set<Domino * Cell * Cell>
        Cells : PipCount[(*row*), (*column*)]
    }

    member board.Item(cell) =
        board.Cells[cell.Row, cell.Column]

module Board =

    [<Literal>]
    let empty : PipCount = -1

    let create numRows numColumns =
        {
            // Dominoes = Set.empty
            Cells = Array2D.create numRows numColumns empty
        }

    let isEmpty (board : Board) cell =
        board[cell] = empty

    let place domino cellLeft cellRight board =
        assert(Cell.areAdjacent cellLeft cellRight)
        assert(isEmpty board cellLeft)
        assert(isEmpty board cellRight)
        let cells = Array2D.copy board.Cells
        cells[cellLeft.Row, cellLeft.Column] <- domino.Left
        cells[cellRight.Row, cellRight.Column] <- domino.Right
        {
            Cells = cells
            (*
            Dominoes =
                board.Dominoes
                    |> Set.add (domino, cellLeft, cellRight)
            *)
        }
