package net.methvin.sudoku

import io.Source

/**
 * The main entry point for the Sudoku solver app
 *
 * The program gets sudoku puzzles from individual files whose names are given in arguments. If
 * the string "-" is passed as an argument, the program reads from standard in instead of a file.
 *
 * When solving multiple puzzles, the output is printed with two blank lines between the
 * solutions to separate them.
 */
object Sudoku extends App {
  args.map {
    case "-" => Source.stdin
    case f => Source.fromFile(f)
  }.foreach { source =>
    val board = Board.fromString(source.mkString)
    val solved = board.solve
    println("\n" + solved.getOrElse("No Solution") + "\n")
  }
}
