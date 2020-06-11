package leetcode.thirtydaysofcode.weekthree

import scala.collection.immutable.HashSet

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

object NumberOfIslands extends App {

  def numIslands(grid: Array[Array[Char]]): Int = {
    val directions = Vector((0,1), (0,-1), (1,0), (-1,0))
    if (grid.isEmpty || grid(0).isEmpty) return 0

    val (width, height) = (grid(0).length, grid.length)
    println(s"width $width height $height")

    var islands = 0

    for {
      x <- (0 until width)
      y <- (0 until height)
      if grid(y)(x) == '1'
    } {
      islands += 1
      dfs(x, y)
    }

    def dfs(x:Int, y:Int):Unit =
      if (x >= 0 && x < width && y >= 0 && y < height && grid(y)(x) == '1'){
        grid(y)(x) = '0'
        for (d <- directions) dfs(x+d._1, y+d._2)
      }

    islands
  }

  println(
    numIslands(
      Array(
        Array('1','1','1'),
        Array('0','1','0'),
        Array('1','1','1'),
      )
    )
  )

  println(
    numIslands(
      Array(
        Array('1','1','1','1','0'),
        Array('1','1','0','1','0'),
        Array('1','1','0','0','0'),
        Array('0','0','0','0','0')
      )
    )
  )

}

