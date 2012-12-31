package net.methvin.sudoku

/**
 * A cell on the Sudoku board.
 *
 * @author Greg Methvin (greg@methvin.net)
 *
 * @param row the row of the cell
 * @param col the column of the cell
 * @param values the set of possible values this cell can have
 */
sealed abstract class Cell(val row: Int, val col: Int, val values: Set[Int]) {

  /** The value of this cell, if it is set */
  val value: Option[Int]

  /** Whether a value has been set for this cell */
  lazy val isSolved: Boolean = value.isDefined

  /**
   * Remove a possible value from this cell
   *
   * @param v the value to remove
   * @return a new cell which does not have the given number as a possible value
   */
  def -(v: Int): Cell

  /** The coordinate on the board (row, col) */
  val coord: (Int, Int) = (row, col)

  /** The subsquare number of this cell, zero-indexed, left to right, top to bottom */
  val squareNumber: Int = row / Board.SquareDim * Board.SquareDim + col / Board.SquareDim

  /**
   * Check if this cell is in the same region (i.e could have the same number) as another cell
   *
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
  override val row: Int, override val col: Int, private val cellValue: Int)
  extends Cell(row, col, Set(cellValue)) {

  val value = Some(cellValue)

  def -(v: Int): SolvedCell = {
    // since this cell is solved, we cannot remove the value it contains
    if (v == cellValue)
      throw new RuntimeException("(%d, %d) = %d has already been solved!".format(row, col, v))
    // removing any other value is a no-op
    this
  }
}

/**
 * An unsolved cell
 */
final case class UnsolvedCell(
  override val row: Int, override val col: Int, override val values: Set[Int] = Board.AllValues)
  extends Cell(row, col, values) {

  val value = None

  def -(v: Int): UnsolvedCell =
    UnsolvedCell(row, col, values - v)
}
