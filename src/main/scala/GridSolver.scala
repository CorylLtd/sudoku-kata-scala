object GridSolver {

  def solve(grid: Grid): Grid = {

    def solveGrid(grid: Grid): Grid =
      if (grid.unassignedCount == 0) grid else {
        val potentialMoves = grid.potentialMoves()
        if (potentialMoves.isEmpty) null else {
          val nextMove = potentialMoves.head
          solveGridList(nextMove.possibleValues.map(pv =>
            grid.setSquare(nextMove.row, nextMove.col, pv)))
        }
      }

    def solveGridList(grids: Vector[Grid]): Grid =
      if (grids.isEmpty) null else solveGrid(grids.head) match {
        case null => solveGridList(grids.tail)
        case solution => solution
      }


    solveGridList(Vector(grid))
  }
}
