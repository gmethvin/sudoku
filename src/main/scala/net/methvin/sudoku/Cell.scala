package net.methvin.sudoku

/**
 * A cell on the Sudoku board.
 *
 * @author Greg Methvin (greg@methvin.net)
 */
sealed abstract class Cell(_row: Int, _col: Int) {

  /** The row on the board */
  val row: Int = _row

  /** The column on the board */
  val col: Int = _col

  /** The coordinate on the board (row, col) */
  val coord: (Int, Int) = (_row, _col)

  /** The value of this cell, if it is set */
  val value: Option[Int]

  /** The possible values this cell can have */
  val values: Set[Int]

  /** Whether a value has been set for this cell */
  val isSolved: Boolean

  /**
   * Remove a possible value from this cell
   *
   * @param v the value to remove
   * @return a new cell which does not have the given number as a possible value
   */
  def -(v: Int): Cell

  /**
   * The subsquare number of this cell, zero-indexed, left to right, top to bottom
   */
  val squareNumber: Int = row / Board.SquareDim * Board.SquareDim + col / Board.SquareDim

  /**
   * Check if this cell is in the same region (i.e could have the same number) as another cell
   * @param cell the other cell to check
   * @return true if this cell has the same row, column, or subsquare, false otherwise
   */
  def isSameRegion(cell: Cell): Boolean =
    cell.row == row || cell.col == col || cell.squareNumber == squareNumber

  /**
   * Check if the given value could be a solution for this cell.
   *
   * @param v the possible value to check
   * @return true if the value is possible in this square, false otherwise
   */
  def hasPossibleValue(v: Int): Boolean =
    values.contains(v)

  override def toString = {
    "(%d, %d) [%s]".format(row, col, values.mkString)
  }
}

/**
 * A solved cell
 */
final case class SolvedCell(
  override val row: Int, override val col: Int, private val cellValue: Int) extends Cell(row, col) {

  val value = Some(cellValue)
  val values = value.toSet
  val isSolved = true

  def -(v: Int): SolvedCell = {
    if (v == cellValue) {
      throw new RuntimeException("(%d, %d) = %d has already been solved!".format(row, col, v))
    } else {
      this
    }
  }
}

/**
 * An unsolved cell
 */
final case class UnsolvedCell(
  override val row: Int, override val col: Int, override val values: Set[Int] = Board.AllValues)
  extends Cell(row, col) {

  val value = None
  val isSolved = false

  def -(v: Int): UnsolvedCell =
    UnsolvedCell(row, col, values - v)
}
