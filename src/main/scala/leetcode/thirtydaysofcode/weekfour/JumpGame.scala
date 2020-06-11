package leetcode.thirtydaysofcode.weekfour

//Given an array of non-negative integers, you are initially positioned at the first index of the array.
//
//Each element in the array represents your maximum jump length at that position.
//
//Determine if you are able to reach the last index.
//
//Example 1:
//
//Input: [2,3,1,1,4]
//Output: true
//Explanation: Jump 1 step from index 0 to 1, then 3 steps to the last index.
//Example 2:
//
//Input: [3,2,1,0,4]
//Output: false
//Explanation: You will always arrive at index 3 no matter what. Its maximum
//jump length is 0, which makes it impossible to reach the last index.
object JumpGame extends App {

  def canJump(nums: Array[Int]): Boolean = {

    def helper(currentIndex: Int, lastIndex: Int, indices: Set[Int]): Boolean =
      // we stop... when we reach the end / run out of possible solutions
      if (currentIndex == lastIndex) true
      else {
        val indexToProcess = nums(currentIndex)
        // for each number we see - we explore all
        // the paths for jumps of size 1 to n
        val indicesAfterJump = (1 to indexToProcess).map(_ + currentIndex).filter(_ <= lastIndex)
        val newIndices = indices - currentIndex ++ indicesAfterJump
        if (newIndices.isEmpty) return false
        else helper(newIndices.max, lastIndex, newIndices - newIndices.max)
      }

    helper(0, nums.length - 1, Set.empty[Int])
  }

  println(canJump(Array(2, 0)))
}
