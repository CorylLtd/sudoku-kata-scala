case class Move(row: Int, col: Int, possibleValues: Vector[Int])

case class Grid(numbers: Vector[Int]) {
  val GRID_DIMENSION = 9
  val SQUARE_SIZE: Int = Math.sqrt(GRID_DIMENSION).toInt

  require(numbers.length == GRID_DIMENSION * GRID_DIMENSION,
    s"Grid must be a $GRID_DIMENSION x $GRID_DIMENSION value, i.e. ${GRID_DIMENSION * GRID_DIMENSION} numbers")

  override def toString: String =
    numbers.sliding(GRID_DIMENSION, GRID_DIMENSION).mkString("\n")

  /**
    * Get all numbers on a row
    *
    * @param rowNumber - index of the row (0-8)
    */
  def row(rowNumber: Int): Vector[Int] =
    numbers.slice(rowNumber * GRID_DIMENSION, (rowNumber + 1) * GRID_DIMENSION)

  /**
    * Get all numbers in column
    *
    * @param colNumber - index of the column
    */
  def col(colNumber: Int): Vector[Int] = {
    def colBuilder(sourceNumbers: Vector[Int], acc: Vector[Int]): Vector[Int] = {
      if (acc.length == GRID_DIMENSION) acc else
        colBuilder(sourceNumbers.drop(GRID_DIMENSION), acc :+ sourceNumbers.head)
    }

    colBuilder(numbers.drop(colNumber), Vector[Int]())
  }

  /**
    * Get all numbers in a square
    *
    * @param row - index of the square row (0-2)
    * @param col - index of the square col (0-SQUARE_SIZE)
    */
  def group(row: Int, col: Int): Vector[Int] = {
    def squareBuilder(sourceNumbers: Vector[Int], acc: Vector[Int]): Vector[Int] = {
      if (acc.length == GRID_DIMENSION) acc else
        squareBuilder(sourceNumbers.drop(GRID_DIMENSION), acc ++ sourceNumbers.take(SQUARE_SIZE))
    }

    squareBuilder(numbers.drop((GRID_DIMENSION * SQUARE_SIZE * row) + SQUARE_SIZE * col), Vector[Int]())
  }

  private def offset(row: Int, col: Int): Int = (GRID_DIMENSION * row) + col

  /**
    * Return a new grid with the given square set to the given value
    * @param row - row of square to set
    * @param col - col of square to set
    * @param value - new value
    */
  def setSquare(row: Int, col: Int, value: Int): Grid =
    Grid(numbers.updated(offset(row, col), value))

  /**
    * Get the value of the given square
    * @param row - row of square
    * @param col - col of square
    * @return
    */
  def getSquare(row: Int, col: Int): Int =
    numbers(offset(row, col))

  /**
    * Get the number of squares that have no value
    */
  def unassignedCount(): Int = numbers.count(_ == 0)

  /**
    * Get all the moves possible for the given cell
    *
    * @param row - row of cell
    * @param col - col of cell
    * @return - a move that includes all possible values
    */
  def potentialMove(row: Int, col: Int): Move =
    Move(row, col, (1 to GRID_DIMENSION).toVector
      .diff(this.row(row))
      .diff(this.col(col))
      .diff(group(row / SQUARE_SIZE, col / SQUARE_SIZE))
    )

  /**
    * Get all potential moves and their potential values for this grid
    */
  def potentialMoves(): Vector[Move] = {
    for {
      row <- 0 until GRID_DIMENSION
      col <- 0 until GRID_DIMENSION if getSquare(row, col) == 0
    } yield potentialMove(row, col)
  }.sortBy(mv => mv.possibleValues.length)
    .filterNot(mv => mv.possibleValues.isEmpty)
    .toVector
}

object Grid {
  def apply(numbers: Int*): Grid = new Grid(numbers.toVector)
}

