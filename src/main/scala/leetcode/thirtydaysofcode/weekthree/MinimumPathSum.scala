package leetcode.thirtydaysofcode.weekthree

//Given a m x n grid filled with non-negative numbers, find a path from top left to bottom right
// which minimizes the sum of all numbers along its path.
//
//Note: You can only move either down or right at any point in time.
//
//Example:
//
//Input:
//[
//[1,3,1],
//[1,5,1],
//[4,2,1]
//]
//Output: 7
//Explanation: Because the path 1→3→1→1→1 minimizes the sum.

object MinimumPathSum extends App {

  def minPathSum(grid: Array[Array[Int]]): Int = {
    if(grid.isEmpty) return 0
    for {
      i <- grid.indices
      j <- grid(i).indices
      if !(i == 0 && j == 0)
    } yield {
      println(s"i is $i, j is $j")
      val left = if(j > 0) grid(i)(j-1) else Int.MaxValue
      val up = if(i > 0) grid(i-1)(j) else Int.MaxValue
      grid(i)(j) += Math.min(left, up)
    }

    grid.last.last
  }

  println(minPathSum(Array(
    Array(1, 3, 1),
    Array(1, 5, 1),
    Array(4, 2, 1)
  )))
}
