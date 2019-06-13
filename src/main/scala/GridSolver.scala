object GridSolver {

  def solve(grid: Grid): Grid = {

    def solveGrid(grid: Grid): Grid =
      if (grid.isFull) grid else grid.nextPotentialMove match {
        case Some(move)  =>
          findSolution(move.possibleValues.map(
            grid.setSquare(move.row, move.col, _)))
        case None => null
      }

    def findSolution(grids: List[Grid]): Grid =
      if (grids.isEmpty) null else solveGrid(grids.head) match {
        case null => findSolution(grids.tail)
        case solution => solution
      }

    findSolution(List(grid))
  }
}
