object GridSolver {

  def solve(grid: Grid): Grid = {

    def solveGrid(grid: Grid): Grid =
      if (grid.isFull) grid else grid.potentialMoves() match {
        case head +: _ =>
          findSolution(head.possibleValues.map(
            grid.setSquare(head.row, head.col, _)))
        case _ => null
      }

    def findSolution(grids: List[Grid]): Grid =
      if (grids.isEmpty) null else solveGrid(grids.head) match {
        case null => findSolution(grids.tail)
        case solution => solution
      }

    findSolution(List(grid))
  }
}
