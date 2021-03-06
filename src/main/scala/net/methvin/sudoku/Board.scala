package net.methvin.sudoku

import collection.IndexedSeqOptimized
import collection.generic.CanBuildFrom
import collection.mutable.{ArrayBuffer, Builder}

/**
 * An immutable data structure representing a Sudoku board.
 *
 * @author Greg Methvin (greg@methvin.net)
 *
 * @param cells A sequence of cells (left to right, top to bottom) of the board
 */
final class Board private(val cells: Seq[Cell]) extends IndexedSeq[Cell] with IndexedSeqOptimized[Cell, Board] {

  /**
   * A list of all cells which have not been solved, i.e. cells for which a value has not been set.
   *
   * Note that a cell may have only one possible value and not be considered "solved", for example in an initial board.
   * Cells are only converted from unsolved to solved within the solve method.
   */
  val unsolvedCells: Seq[Cell] = cells.filterNot(_.isSolved)

  /**
   * A boolean indicating whether there are unsolved cells left on the board.
   */
  val isSolved: Boolean = unsolvedCells.isEmpty

  /**
   * Get a cell at a particular location on the board.
   *
   * @param loc the location coordinate (row, col)
   * @return the cell on the board at that location
   */
  def apply(loc: (Int, Int)): Cell = {
    val (row, col) = loc
    cells(Board.BoardDim * row + col)
  }

  /**
   * Get the ith cell in the board.
   *
   * @param i the index of the cell to get
   */
  def apply(i: Int) = cells(i)

  /**
   * Solve a cell on the board by setting a value for it
   *
   * @param loc the coordinate of the location to change
   * @param value the value to set it to
   * @return a new board which has the given cell solved and set to the value given
   */
  def solveCell(loc: (Int, Int), value: Int): Board = {
    val cell = this(loc)
    if (!cell.hasPossibleValue(value)) {
      throw new IllegalArgumentException(s"(${cell.row}, ${cell.col}) = $value is invalid")
    }
    map { cc =>
      if (cc == cell)
        SolvedCell(loc, value)
      else if (cc.isSameRegion(cell))
        cc - value
      else
        cc
    }
  }

  /**
   * Solve the board.
   *
   * @return a new solved board containing all solved cells.
   */
  def solved: Option[Board] = {
    if (isSolved) {
      Some(this)
    } else {
      val bestCell = unsolvedCells.minBy(_.values.size)
      bestCell.values.toStream.map { value =>
        solveCell(bestCell.loc, value).solved
      }.flatten.headOption
    }
  }

  override lazy val toString: String = {
    val squareRow = Seq.fill(Board.SquareDim)("%s").mkString
    val fullRow = Seq.fill(Board.BoardDim / Board.SquareDim)(squareRow).mkString(" ")
    val rowOfSquares = Seq.fill(Board.SquareDim)(fullRow).mkString("\n")
    val boardFormat = Seq.fill(Board.BoardDim / Board.SquareDim)(rowOfSquares).mkString("\n\n")
    boardFormat.format(map(_.value.getOrElse(Board.EmptyChar)): _*)
  }

  override protected[this] def newBuilder: Builder[Cell, Board] = Board.newBuilder

  override def length: Int = cells.length
}

object Board {

  /** The dimension of a single square */
  val SquareDim = 3

  /** The dimension of the whole board */
  val BoardDim = SquareDim * SquareDim

  /** The size of the whole board (width*height) */
  val BoardSize = BoardDim * BoardDim

  /** A set of all possible values for this board */
  val AllValues: Set[Int] = (1 to Board.BoardDim).toSet

  /** The character used for an empty space in the toString of the board */
  val EmptyChar = '*'

  /** An empty board */
  val emptyBoard: Board =
    Board(for (row <- 0 until BoardDim; col <- 0 until BoardDim) yield UnsolvedCell(row -> col))

  /**
   * Create a board from a set of Sudoku cells
   *
   * @param cells a collection of cells from which to create the board
   * @return a board constructed from these cells
   */
  def apply(cells: Traversable[Cell]): Board =
    new Board(cells.toSeq.sortBy(cell => cell.row * BoardDim + cell.col))

  /**
   * Convert a sequence of values into a Sudoku board.
   *
   * @param values an ordered sequence of values for the board (left to right, top to bottom)
   * @return a board constructed from this sequence with the proper cell values set
   */
  def apply(values: Seq[Option[Int]]): Board = {
    val locations = (0 until BoardSize) map {
      i => (i / BoardDim, i % BoardDim)
    }
    values.zip(locations).foldLeft(emptyBoard) {
      case (board, (Some(x), loc)) => board.solveCell(loc, x)
      case (board, _) => board
    }
  }

  /**
   * Convert a string to a Sudoku board.
   *
   * @param str a string of characters with optional whitespace. Numbers 1-9 represent values and all other
   *            non-whitespace characters are blank cells.
   * @return a board constructed from these characters
   */
  def apply(str: String): Board = apply(getBoardValues(str))

  protected def newBuilder: Builder[Cell, Board] =
    new ArrayBuffer[Cell] mapResult Board.apply

  implicit def canBuildFrom: CanBuildFrom[Board, Cell, Board] =
    new CanBuildFrom[Board, Cell, Board] {
      def apply: Builder[Cell, Board] = newBuilder
      def apply(from: Board): Builder[Cell, Board] = newBuilder
    }

  /** Get the values for the board from a string. */
  private def getBoardValues(boardString: String): Seq[Option[Int]] = {
    val values = boardString.filterNot(_.isWhitespace).map(Character.digit(_, 10) match {
      // only numbers 1-9 are counted as values; no other characters
      case d if d > 0 => Some(d)
      case _ => None
    })
    if (values.length != BoardSize)
      throw new IllegalArgumentException("Sudoku puzzle must contain " + BoardSize + " cells, not " + values.length)
    values
  }
}
