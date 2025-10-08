namespace Pips

type Board =
    {
        Dominoes : Set<Domino * Cell * Cell>
        Cells : Option<PipCount>[(*row*), (*column*)]
    }

    member board.Item(cell) =
        board.Cells[cell.Row, cell.Column]

module Board =

    let create numRows numColumns =
        {
            Dominoes = Set.empty
            Cells = Array2D.zeroCreate numRows numColumns
        }

    let tryGetPipCount cell (board : Board) =
        board[cell]

    let isEmpty (board : Board) cell =
        board[cell].IsNone

    let place domino cellLeft cellRight board =
        assert(Cell.adjacent cellLeft cellRight)
        assert(isEmpty board cellLeft)
        assert(isEmpty board cellRight)
        let cells = Array2D.copy board.Cells
        cells[cellLeft.Row, cellLeft.Column] <- Some domino.Left
        cells[cellRight.Row, cellRight.Column] <- Some domino.Right
        {
            Cells = cells
            Dominoes =
                board.Dominoes
                    |> Set.add (domino, cellLeft, cellRight)
        }
