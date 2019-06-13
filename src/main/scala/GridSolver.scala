object GridSolver {

  def solve(grid: Grid): Grid = {

    def solveGrid(grid: Grid): Grid =
      if (grid.isFull) grid else grid.potentialMoves() match {
        case head +: _ =>
          solveGridList(head.possibleValues.map(
            grid.setSquare(head.row, head.col, _)))
        case _ => null
      }


    def solveGridList(grids: List[Grid]): Grid =
      if (grids.isEmpty) null else solveGrid(grids.head) match {
        case null => solveGridList(grids.tail)
        case solution => solution
      }


    solveGridList(List(grid))
  }
}
