package net.methvin.sudoku

import io.Source

/**
 * The main entry point for the Sudoku solver app
 *
 * The program gets sudoku puzzles from individual files whose names are given in arguments. If
 * the string "-" is passed as an argument, the program reads from standard in instead of a file.
 *
 * When solving multiple puzzles, we first print the board for each puzzle, then the solution.
 */
object Sudoku extends App {
  args map {
    case "-" => Source.stdin
    case f => Source.fromFile(f)
  } foreach { source =>
    val board = Board(source.mkString)
    val solution = board.solve.getOrElse("No Solution")
    println(s"\nBoard:\n$board\n\nSolution:\n$solution\n")
  }
}
