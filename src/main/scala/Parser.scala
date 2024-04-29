import SudokuTable.SudokuTable
import scala.util.matching.Regex

object Parser {
  val BLOCK_VER_SEP = "|"
  val CELL_SEPARATOR = " "
  val BLOCK_HOR_SEP = "+-------+-------+-------+"

  def readTable(filename: String): SudokuTable = {
    val content = FileReader.readLines(filename)

    val lines = content.split("\n").toList.filter(!_.equals(BLOCK_HOR_SEP))
    val pattern: Regex = "\\| (.) (.) (.) \\| (.) (.) (.) \\| (.) (.) (.) \\|".r

    for
      line <- lines
      matches <- pattern.findAllMatchIn(line)
    yield (1 to 9).toList.map(i => matches.group(i)).map {
      case " " => None
      case a =>
        try Some(a.toInt) catch
          case e: Exception => throw new RuntimeException(s"Failed to parse: $a")
    }
  }
}

