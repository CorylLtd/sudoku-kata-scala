object Solver extends App {

  val grid = Grid(
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
  println(grid)

}

case class Grid(numbers: Vector[Int]) {
  require(numbers.length == 81, "Grid must be a 9x9 value, i.e. 81 numbers")

  /**
    * Get all 9 numbers on a row
    *
    * @param rowNumber - index of the row (0-8)
    * @return - list of 9 numbers
    */
  def row(rowNumber: Int): Vector[Int] =
    numbers.slice(rowNumber * 9, (rowNumber + 1) * 9)

  /**
    * Get all 9 numbers in column
    *
    * @param colNumber - index of the column (0-8)
    * @return - list of 9 numbers
    */
  def col(colNumber: Int): Vector[Int] = {
    def colBuilder(sourceNumbers: Vector[Int], acc: Vector[Int]): Vector[Int] = {
      if (acc.length == 9) acc else
        colBuilder(sourceNumbers.drop(9), acc :+ sourceNumbers.head)
    }

    colBuilder(numbers.drop(colNumber), Vector[Int]())
  }

  /** 1
    * Get all 9 numbers in a square
    *
    * @param row - index of the square row (0-2)
    * @param col - index of the square col (0-3)
    * @return - list of 9 numbers
    */
  def square(row: Int, col: Int): Vector[Int] = {
    def squareBuilder(sourceNumbers: Vector[Int], acc: Vector[Int]): Vector[Int] = {
      if (acc.length == 9) acc else
        squareBuilder(sourceNumbers.drop(9), acc ++ sourceNumbers.take(3))
    }

    squareBuilder(numbers.drop((3 * 3 * 3 * row) + 3 * col), Vector[Int]())
  }
}

object Grid {
  def apply(numbers: Int*): Grid = new Grid(numbers.toVector)
}