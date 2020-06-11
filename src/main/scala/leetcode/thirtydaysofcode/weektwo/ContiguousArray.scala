package leetcode.thirtydaysofcode.weektwo

//Given a binary array, find the maximum length of a contiguous subarray with equal number of 0 and 1.
//
//Example 1:
//Input: [0,1]
//Output: 2
//Explanation: [0, 1] is the longest contiguous subarray with equal number of 0 and 1.
//Example 2:
//Input: [0,1,0]
//Output: 2
//Explanation: [0, 1] (or [1, 0]) is a longest contiguous subarray with equal number of 0 and 1.
//Note: The length of the given binary array will not exceed 50,000.


object ContiguousArray extends App {
  def findMaxLength(nums: Array[Int]): Int = {
    nums
      .foldLeft((Map(0 -> -1), 0, 0, 0)) { (soFar, next) =>
        val (encountered, currentIndex, currentCount, maxLength) = soFar
        val newCount = currentCount + (if (next == 1) 1 else -1)
        val newMax = Math.max(maxLength, currentIndex - encountered.getOrElse(newCount, currentIndex))
        (
          if (encountered.contains(newCount)) encountered else encountered + (newCount -> currentIndex),
          currentIndex + 1,
          newCount,
          newMax
        )
      }._4
  }

  println(findMaxLength(Array(0, 0, 1, 0, 0, 0, 1, 1)))
  println(findMaxLength(Array(0, 1)))
  println(findMaxLength(Array(0, 1, 0, 1)))
}
