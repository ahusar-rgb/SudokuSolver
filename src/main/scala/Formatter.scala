import Parser.{BLOCK_HOR_SEP, BLOCK_VER_SEP, CELL_SEPARATOR}
import SudokuTable.SudokuTable

object Formatter {

  def formatTable(sudokuTable: SudokuTable): String = {
    val builder = StringBuilder()

    var lineCounter = 0
    for (line <- sudokuTable) {
      if (lineCounter % 3 == 0) {
        builder.append(BLOCK_HOR_SEP + "\n")
      }

      var numberCounter = 0
      var separatorCount = 0
      for (number <- line) {
        if (numberCounter % 3 == 0) {
          builder.append(BLOCK_VER_SEP + " ")
        }
        builder.append(if number.isEmpty then " " else number.get.toString).append(CELL_SEPARATOR)
        numberCounter += 1

        if (numberCounter == 9) {
          builder.append(BLOCK_VER_SEP)
        }
      }
      builder.append("\n")
      lineCounter += 1
    }

    builder.append(BLOCK_HOR_SEP).toString()
  }
}
