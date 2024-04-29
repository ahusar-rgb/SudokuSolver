object SudokuTable {
  private type Cell = Option[Int]
  type SudokuTable = List[List[Cell]]

  extension (sudokuTable: SudokuTable)
    def updatedTable(x: Int, y: Int, a: Int): SudokuTable =
     sudokuTable.updated(y, sudokuTable(y).updated(x, Some(a)))

  extension (sudokuTable: SudokuTable)
    def isValid: Boolean = {
      lazy val boxList = for
        y <- (0 until 9 by 3).toList
        x <- (0 until 9 by 3).toList
      yield sudokuTable.distinctInArea(x, x + 2, y, y + 2)

      lazy val vLineList = for
        i <- (0 until 9).toList
      yield sudokuTable.distinctInArea(i, i, 0, 8)

      lazy val hLineList = for
        i <- (0 until 9).toList
      yield sudokuTable.distinctInArea(0, 8, i, i)

      boxList.forall(identity) && hLineList.forall(identity) && vLineList.forall(identity)
    }

  extension (sudokuTable: SudokuTable)
    def distinctInArea(x0: Int, x: Int, y0: Int, y: Int): Boolean = {
      val set = collection.mutable.Set[Int]()
      var y_c = y0
      while (y_c <= y) {
        var x_c = x0
        while (x_c <= x) {
          val number = sudokuTable(y_c)(x_c)
          if (number.isDefined) {
            if (set.contains(number.get)) {
              return false
            }
            set.add(number.get)
          }
          x_c += 1
        }
        y_c += 1
      }
      true
    }
}
