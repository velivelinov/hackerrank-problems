package leetcode

//A robot is located at the top-left corner of a m x n grid (marked 'Start' in the diagram below).
//
//The robot can only move either down or right at any point in time.
// The robot is trying to reach the bottom-right corner of the grid (marked 'Finish' in the diagram below).
//
//How many possible unique paths are there?
//
//Example 1:
//
//Input: m = 3, n = 2
//Output: 3
//Explanation:
//From the top-left corner, there are a total of 3 ways to reach the bottom-right corner:
//1. Right -> Right -> Down
//2. Right -> Down -> Right
//3. Down -> Right -> Right
//Example 2:
//
//Input: m = 7, n = 3
//Output: 28

object UniquePaths extends App {

  def uniquePaths(m: Int, n: Int): Int = {
    val solutions = scala.collection.mutable.Map[(Int, Int), Int]()

    def helper(row: Int, column: Int): Int =
      if (row == n - 1 && column == m - 1) 1
      else {
        solutions.getOrElse((row, column), {
          val right = if (column + 1 > m) 0 else helper(row, column + 1)
          val down = if (row > n) 0 else helper(row + 1, column)

            solutions((row, column)) = right + down
            right + down
        })
      }

    helper(0, 0)
  }

}
