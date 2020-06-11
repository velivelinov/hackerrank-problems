package others

import scala.collection.immutable.HashSet

//Given an unsorted array of integers, find the length of the longest consecutive elements sequence.
//
//Your algorithm should run in O(n) complexity.
//
//Example:
//
//Input: [100, 4, 200, 1, 3, 2]
//Output: 4
//Explanation: The longest consecutive elements sequence is [1, 2, 3, 4]. Therefore its length is 4.

object LongestSequence extends App {

  @scala.annotation.tailrec
  def findLongestStreak(numbers: Set[Int], current: Int, streak: Int = 1): Int =
    if (numbers.contains(current + 1)) findLongestStreak(numbers - (current + 1), current + 1, streak + 1)
    else streak

  def longestConsecutive(nums: Array[Int]): Int = {
    val numsSet = HashSet() ++ nums
    val startingNumbers = nums.filter(number => !numsSet.contains(number - 1))

      startingNumbers.foldLeft(0) { (longestStreak, nextStart) =>
        Math.max(longestStreak, findLongestStreak(numsSet, nextStart))
      }
  }

  println(longestConsecutive(Array(100, 2, 4, 3, 1)))

}
