package leetcode.thirtydaysofcode.weekthree

// Given a 2d grid map of '1's (land) and '0's (water), count the number of islands.
// An island is surrounded by water and is formed by connecting adjacent lands horizontally or vertically.
// You may assume all four edges of the grid are all surrounded by water.
//
//Example 1:
//
//Input:
//11110
//11010
//11000
//00000
//
//Output: 1
//Example 2:
//
//Input:
//11000
//11000
//00100
//00011
//
//Output: 3
object NumberOfIslandsPractice extends App {

  def numIslands(grid: Array[Array[Char]]): Int = {
    val directions = Vector((0, 1), (0, -1), (1, 0), (-1, 0))
    if (grid.isEmpty || grid(0).isEmpty) return 0

    val width = grid(0).length
    val height = grid.length

    var islands = 0

    for {
      r <- (0 until height)
      c <- (0 until width)
      if (grid(r)(c) == '1')
    } yield {
      islands += 1
      dfs(r, c)
    }

    def dfs(row: Int, col: Int): Unit =
      if (row > -1 && row < height && col > -1 && col < width && grid(row)(col) == '1') {
        grid(row)(col) = 0
        for (d <- directions) {
          dfs(row + d._1, col + d._2)
        }
      }

    islands
  }
}
