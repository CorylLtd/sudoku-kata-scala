import org.scalatest.FunSuite

class GridSolverTests extends FunSuite {

  val easyGrid = Grid(
    5, 3, 0, 0, 7, 0, 0, 0, 0,
    6, 0, 0, 1, 9, 5, 0, 0, 0,
    0, 9, 8, 0, 0, 0, 0, 6, 0,
    8, 0, 0, 0, 6, 0, 0, 0, 3,
    4, 0, 0, 8, 0, 3, 0, 0, 1,
    7, 0, 0, 0, 2, 0, 0, 0, 6,
    0, 6, 0, 0, 0, 0, 2, 8, 0,
    0, 0, 0, 4, 1, 9, 0, 0, 5,
    0, 0, 0, 0, 8, 0, 0, 7, 9
  )

  var mediumGrid = Grid(
    0, 4, 2, 7, 9, 0, 0, 0, 0,
    0, 3, 0, 0, 8, 0, 0, 6, 0,
    8, 0, 5, 1, 0, 0, 0, 0, 0,
    0, 0, 4, 0, 7, 0, 0, 0, 0,
    3, 0, 8, 0, 4, 0, 2, 0, 9,
    0, 0, 0, 0, 3, 0, 8, 0, 0,
    0, 0, 0, 0, 0, 4, 9, 0, 7,
    0, 2, 0, 0, 5, 0, 0, 4, 0,
    0, 0, 0, 0, 6, 7, 3, 2, 0
  )

  var hardGrid = Grid(
    0, 0, 0, 0, 0, 8, 0, 9, 0,
    1, 9, 0, 3, 0, 0, 4, 0, 0,
    0, 0, 0, 0, 0, 7, 0, 5, 6,
    0, 0, 9, 0, 0, 0, 5, 0, 4,
    2, 6, 0, 0, 0, 0, 0, 1, 9,
    4, 0, 3, 0, 0, 0, 6, 0, 0,
    5, 3, 0, 2, 0, 0, 0, 0, 0,
    0, 0, 1, 0, 0, 3, 0, 2, 7,
    0, 8, 0, 1, 0, 0, 0, 0, 0
  )

  var evilGrid = Grid(
    0, 1, 0, 0, 0, 0, 0, 0, 8,
    6, 0, 4, 0, 1, 0, 0, 0, 0,
    0, 0, 0, 8, 0, 2, 6, 0, 0,
    4, 0, 2, 0, 0, 5, 0, 0, 9,
    0, 0, 0, 0, 4, 0, 0, 0, 0,
    3, 0, 0, 9, 0, 0, 5, 0, 4,
    0, 0, 1, 7, 0, 8, 0, 0, 0,
    0, 0, 0, 0, 3, 0, 1, 0, 2,
    9, 0, 0, 0, 0, 0, 0, 5, 0
  )

  private def assertSolved(solution: Grid): Unit = {
    assert(solution != null, "Solution must be found")
    assert(solution.isFull, "Solution must have no unassigned squares")
    println(solution)
  }

  test("Should be able to solve 'easy' puzzle") {
    assertSolved(GridSolver.solve(easyGrid))
  }

  test("Should be able to solve 'medium' puzzle") {
    assertSolved(GridSolver.solve(mediumGrid))
  }

  test("Should be able to solve 'hard' puzzle") {
    assertSolved(GridSolver.solve(hardGrid))
  }

  test("Should be able to solve 'evil' puzzle") {
    assertSolved(GridSolver.solve(evilGrid))
  }
}
