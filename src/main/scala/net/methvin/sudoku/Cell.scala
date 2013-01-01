package net.methvin.sudoku

/**
 * A cell on the Sudoku board.
 *
 * @author Greg Methvin (greg@methvin.net)
 *
 * @param loc the location (row, col) of this cell
 * @param values the set of possible values this cell can have
 */
sealed abstract class Cell(val loc: (Int, Int), val values: Set[Int]) {

  /** The row of this cell */
  val row: Int = loc._1

  /** The column of this cell */
  val col: Int = loc._2

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

  /** The subsquare number of this cell, zero-indexed, left to right, top to bottom */
  val squareNumber: Int = row / Board.SquareDim * Board.SquareDim + col / Board.SquareDim

  /**
   * Check if this cell is in the same region (i.e could have the same number) as another cell
   *
   * @param cell the other cell to check
   * @return true if this cell has the same row, column, or subsquare, false otherwise
   */
  def isSameRegion(cell: Cell): Boolean =
    cell.loc == loc || cell.squareNumber == squareNumber

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
final case class SolvedCell(override val loc: (Int, Int), private val cellValue: Int)
  extends Cell(loc, Set(cellValue)) {

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
final case class UnsolvedCell(override val loc: (Int, Int),
  override val values: Set[Int] = Board.AllValues) extends Cell(loc, values) {

  val value = None

  def -(v: Int): UnsolvedCell =
    UnsolvedCell(loc, values - v)
}
