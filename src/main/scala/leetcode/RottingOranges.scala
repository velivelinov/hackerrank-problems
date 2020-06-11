package leetcode

import scala.collection.immutable.HashSet

object RottingOranges extends App {

  def orangesRotting(grid: Array[Array[Int]]): Int = {

    def orangesRotting(freshOranges: HashSet[(Int, Int)], minute: Int): Int = {
      if (freshOranges.isEmpty) minute
      else {
        // start next minute
        val newRotten = freshOranges.filter(coord => hasRottenNeighbour(coord._1, coord._2))

        if (newRotten.isEmpty) -1
        else {
          val newFresh = freshOranges -- newRotten
          newRotten.foreach(coordinates => grid(coordinates._1)(coordinates._2) = 2)
          orangesRotting(newFresh, minute + 1)
        }
      }
    }

    def hasRottenNeighbour(row: Int, column: Int): Boolean = {
      Seq(
        (row - 1, column),
        (row + 1, column),
        (row, column - 1),
        (row, column + 1)
      ).exists {
        case (r, c) =>
          r >= 0 && c >= 0 && r < grid.length && c < grid(r).length && grid(r)(c) == 2
      }
    }

    if (grid.isEmpty) -1
    else {
      orangesRotting(
        freshOranges = HashSet[(Int, Int)]((for {
          row <- grid.indices
          column <- grid(row).indices
          if grid(row)(column) == 1
        } yield (row, column)): _*),
        minute = 0
      )
    }
  }

  println(
    orangesRotting(
      Array(
        Array(2, 1, 1),
        Array(1, 1, 0),
        Array(0, 1, 1),
      )
    )
  )
}
