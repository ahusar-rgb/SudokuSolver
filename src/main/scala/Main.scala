import SudokuTable.*

object Main {
  def main(args: Array[String]): Unit = {
    val sudokuTable = Parser.readTable("src/main/resources/table.txt")
    val solved = Solver.solve(sudokuTable)
    println(Formatter.formatTable(solved.get))
  }
}