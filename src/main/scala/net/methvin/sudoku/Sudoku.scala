package net.methvin.sudoku

object Sudoku extends App {
  if (args.isEmpty) {
    System.err.println("First argument should be the Sudoku board string.")
    System.exit(1)
  }
  val board = Board.fromString(args(0))
  println(
    """
      |Unsolved:
      |%s
      |
      |Solved:
      |%s
    """.stripMargin.format(board, board.solve.getOrElse("No Solution")))

}
