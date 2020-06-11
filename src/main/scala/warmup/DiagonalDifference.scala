package warmup

import scala.annotation.tailrec

object DiagonalDifference extends App {

  val input1 = Vector(
    Vector(11, 2, 4),
    Vector(4, 5, 6),
    Vector(10, 8, -12)
  )

  def diagonalDifference(list: Vector[Vector[Int]]): Int = {

    @tailrec
    def helper(currentRow: Int, rows: Int, d1: Int, d2: Int, remainder: Vector[Vector[Int]]): Int = {
      remainder match {
        case IndexedSeq() => Math.abs(d1 - d2)
        case head +: tail => helper(
          currentRow + 1,
          rows,
          d1 + head(currentRow),
          d2 + head(rows - currentRow - 1),
          tail
        )
      }
    }

    // Write your code here
    helper(0, list.size, 0, 0, list)
  }

  println(diagonalDifference(input1))
}
