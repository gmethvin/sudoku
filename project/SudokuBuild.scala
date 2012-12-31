import sbt._
import sbt.Keys._

object SudokuBuild extends Build {

  lazy val sudoku = Project(
    id = "sudoku",
    base = file("."),
    settings = Project.defaultSettings ++ Seq(
      name := "Sudoku",
      organization := "net.methvin.sudoku",
      version := "0.1-SNAPSHOT",
      scalaVersion := "2.10.0"
      // add other settings here
    )
  )
}
