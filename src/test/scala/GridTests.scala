import org.scalatest.FunSuite

class GridTests extends FunSuite {

  val testGrid = Grid(
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

  test("Should return row 0 of grid") {
    val row0 = testGrid.row(0)
    assert(row0 === List(5, 3, 0, 0, 7, 0, 0, 0, 0))
  }

  test("Should return row 1 of grid") {
    val row0 = testGrid.row(1)
    assert(row0 === List(6, 0, 0, 1, 9, 5, 0, 0, 0))
  }

  test("Should return row 8 of grid") {
    val row0 = testGrid.row(8)
    assert(row0 === List(0, 0, 0, 0, 8, 0, 0, 7, 9))
  }

  test("Should return col 0 of grid") {
    val col0 = testGrid.col(0)
    assert(col0 === List(5, 6, 0, 8, 4, 7, 0, 0, 0))
  }

  test("Should return col 1 of grid") {
    val col0 = testGrid.col(1)
    assert(col0 === List(3, 0, 9, 0, 0, 0, 6, 0, 0))
  }

  test("Should return col 8 of grid") {
    val col0 = testGrid.col(8)
    assert(col0 === List(0, 0, 0, 3, 1, 6, 0, 5, 9))
  }

  test("Should get block at 0, 0") {
    val square00 = testGrid.block(0, 0)
    assert(square00 === List(5, 3, 0, 6, 0, 0, 0, 9, 8))
  }

  test("Should get block at 1, 0") {
    val square10 = testGrid.block(1, 0)
    assert(square10 === List(8, 0, 0, 4, 0, 0, 7, 0, 0))
  }

  test("Should get block at 2, 2") {
    val square22 = testGrid.block(2, 2)
    assert(square22 === List(2, 8, 0, 0, 0, 5, 0, 7, 9))
  }

  test("Should set a square value at 0, 0") {
    val newGrid = testGrid.setSquare(0, 0, 8)
    val row0 = newGrid.row(0)
    assert(row0 === List(8, 3, 0, 0, 7, 0, 0, 0, 0))
  }

  test("Should set a square value at 8, 8") {
    val newGrid = testGrid.setSquare(8, 8, 1)
    val square22 = newGrid.block(2, 2)
    assert(square22 === List(2, 8, 0, 0, 0, 5, 0, 7, 1))
  }

  test("Should get square value at 0, 0") {
    val squareValue = testGrid.getSquare(0, 0)
    assert(squareValue === 5)
  }

  test("Should get square value at 8, 8") {
    val squareValue = testGrid.getSquare(8, 8)
    assert(squareValue === 9)
  }

  test("Should determine correct potential moves for 0, 2") {
    val potentialMove = testGrid.potentialMove(0, 2)
    assert(potentialMove.possibleValues === List(1, 2, 4))
  }
}
