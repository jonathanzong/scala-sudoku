object Main {
  def main(args: Array[String]): Unit = {
    val board =
      "..53.....8......2..7..1.5..4....53...1..7...6..32...8..6.5....9..4....3......97.."
        .replace(".", "0")
        .split("")
        .filter(_.length > 0)
        .map { c =>
          val i = c.toInt
          if (i == 0) {
            (1 to 9)
          }
          else {
            List(i)
          }
        }.toList

    val start = System.currentTimeMillis
    val solution = Sudoku.solve(board)
    println((System.currentTimeMillis - start)/1000.0)

    solution match {
      case Some(x) =>
        Sudoku.printGrid(x)
      case None =>
        println("No solution")
    }


  }
}
