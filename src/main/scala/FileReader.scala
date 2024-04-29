import scala.io.Source.fromFile

object FileReader {

  def readLines (filename: String): String =
    val source = fromFile(filename)
    try
      source.mkString
    finally
      source.close()

}
