package net.methvin.sudoku

/**
 * An immutable data structure representing a Sudoku board.
 *
 * @param cells A sequence of cells (left to right, top to bottom) of the board
 *
 * @author Greg Methvin (greg@methvin.net)
 */
final case class Board(cells: Seq[Cell]) {

  /**
   * A list of all cells which have not been solved, i.e. cells for which a value has not been set.
   *
   * Note that a cell may have only one possible value and not be considered "solved", for example
   * in an initial board. Cells are only converted from unsolved to solved within the solve method.
   */
  val unsolvedCells: Seq[Cell] = cells.filterNot(_.isSolved)

  /**
   * A boolean indicating whether there are unsolved cells left on the board.
   */
  val isSolved: Boolean = unsolvedCells.isEmpty

  /**
   * Get a cell at a particular coordinate on the board.
   *
   * @param coord the coordinate (row, col)
   * @return the cell on the board for that coordinate
   */
  def get(coord: (Int, Int)): Cell = {
    val (row, col) = coord
    cells(Board.BoardDim * row + col)
  }

  /**
   * Set a coordinate on the board to a value in a new board.
   *
   * @param coord the coordinate to change
   * @param value the value to set it to
   * @return a new board which is the same as this one, but with that coordinate changed.
   */
  def set(coord: (Int, Int), value: Int): Board = {
    val (r, c) = coord
    val cell = get(coord)
    if (!cell.hasPossibleValue(value)) {
      throw new RuntimeException("(%d, %d) = %d is invalid".format(r, c, value))
    }
    Board(cells.map { cc =>
      if (cc == cell)
        SolvedCell(r, c, value)
      else if (cc.isSameRegion(cell))
        cc - value
      else
        cc
    })
  }

  /**
   * Solve the board.
   *
   * @return a new solved board containing all solved cells.
   */
  def solve: Option[Board] = {
    if (isSolved) {
      Some(this)
    } else {
      val bestCell = unsolvedCells.minBy(_.values.size)
      bestCell.values.toStream.map { value =>
        set(bestCell.coord, value).solve
      }.flatten.headOption
    }
  }

  private lazy val boardFormat = {
    val squareRow = Seq.fill(Board.SquareDim)("%s").mkString
    val fullRow = Seq.fill(Board.BoardDim / Board.SquareDim)(squareRow).mkString(" ")
    val rowOfSquares = Seq.fill(Board.SquareDim)(fullRow).mkString("\n")
    Seq.fill(Board.BoardDim / Board.SquareDim)(rowOfSquares).mkString("\n\n")
  }

  override def toString: String =
    boardFormat.format(cells.map(_.value.getOrElse(Board.EmptyChar)): _*)

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
  val emptyBoard: Board = {
    val indices = 0 until BoardDim
    Board(for (row <- indices; col <- indices) yield UnsolvedCell(row, col))
  }

  /**
   * Convert a string to a Sudoku board.
   *
   * @param str a string of characters containing the numbers for the Sudoku (left to right, top to
   *            bottom), optionally separated by whitespace.
   * @return a board constructed from these characters
   */
  def fromString(str: String): Board = {
    (0 until BoardSize)
      .map(i => (i / BoardDim, i % BoardDim))
      .zip(getBoardValues(str))
      .foldLeft(emptyBoard) {
        case (board, (coord, Some(x))) => board.set(coord, x)
        case (board, _) => board
      }
  }

  /**
   * Get the values for the board from a string.
   */
  private def getBoardValues(boardString: String): Seq[Option[Int]] = {
    val values = boardString.filterNot(_.isWhitespace).map(Character.digit(_, 10) match {
      // only numbers 1-9 are counted as values; no other characters
      case d if d > 0 => Some(d)
      case _ => None
    })
    if (values.length != BoardSize)
      throw new IllegalArgumentException(
        "Sudoku puzzle must contain " + BoardSize + " cells, not " + values.length)
    values
  }

}
