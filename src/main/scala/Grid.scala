case class Move(row: Int, col: Int, possibleValues: List[Int])

case class Grid(numbers: List[Int]) {
  /**
    * Overall size of Sudoku grid
    */
  val GRID_SIZE = 9

  /**
    * Size of 'blocks'
    */
  val BLOCK_SIZE: Int = Math.sqrt(GRID_SIZE).toInt

  require(numbers.length == GRID_SIZE * GRID_SIZE,
    s"Grid must be a $GRID_SIZE x $GRID_SIZE value, i.e. ${GRID_SIZE * GRID_SIZE} numbers")

  override def toString: String =
    numbers.sliding(GRID_SIZE, GRID_SIZE).mkString("\n")

  /**
    * Get all numbers on a row
    *
    * @param rowNumber - index of the row (0-8)
    */
  def row(rowNumber: Int): List[Int] =
    numbers.slice(rowNumber * GRID_SIZE, (rowNumber + 1) * GRID_SIZE)

  /**
    * Get all numbers in column
    *
    * @param colNumber - index of the column
    */
  def col(colNumber: Int): List[Int] =
    numbers.drop(colNumber).sliding(1, GRID_SIZE).flatten.toList

  /**
    * Get all numbers in a square
    *
    * @param row - index of the square row (0-2)
    * @param col - index of the square col (0-SQUARE_SIZE)
    */
  def block(row: Int, col: Int): List[Int] =
    numbers.drop(GRID_SIZE * BLOCK_SIZE * row + BLOCK_SIZE * col)
      .sliding(BLOCK_SIZE, GRID_SIZE).take(BLOCK_SIZE).flatten.toList

  private def offset(row: Int, col: Int): Int = (GRID_SIZE * row) + col

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
  def isFull: Boolean = numbers.count(_ == 0) == 0

  /**
    * Get all the moves possible for the given cell
    *
    * @param row - row of cell
    * @param col - col of cell
    * @return - a move that includes all possible values
    */
  def potentialMove(row: Int, col: Int): Move =
    Move(row, col, (1 to GRID_SIZE).toList
      .diff(this.row(row))
      .diff(this.col(col))
      .diff(block(row / BLOCK_SIZE, col / BLOCK_SIZE))
    )

  /**
    * Get the next potential move and it's potential values for this grid
    */
  def nextPotentialMove: Option[Move] = {
    for {
      row <- 0 until GRID_SIZE
      col <- 0 until GRID_SIZE if getSquare(row, col) == 0
    } yield potentialMove(row, col)
  } .filterNot(mv => mv.possibleValues.isEmpty)
    .sortBy(mv => mv.possibleValues.length)
    .headOption
}

object Grid {
  def apply(numbers: Int*): Grid = new Grid(numbers.toList)
}

