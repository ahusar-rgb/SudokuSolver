import SudokuTable.*

import scala.io.Source.{fromFile, stdin}

object Solver {

  def solve(sudokuTable: SudokuTable): Option[SudokuTable] = {
    def solveFrom(sudokuTable: SudokuTable, index: Int): Option[SudokuTable] = {
      if (!sudokuTable.isValid)
        return None
      if (index == 80)
        return Some(sudokuTable)

      val y = index / 9
      val x = index % 9

      if (sudokuTable(y)(x).isDefined)
        return solveFrom(sudokuTable, index + 1)

      val list =
        for
          i <- (1 to 9).toList
        yield
          solveFrom(sudokuTable.updatedTable(x, y, i), index + 1)

      val solved = list.filter(_.isDefined)
      if solved.nonEmpty then
        Some(solved.head.get)
      else None
    }
    solveFrom(sudokuTable, 0)
  }
}
