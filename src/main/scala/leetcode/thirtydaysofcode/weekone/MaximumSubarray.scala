package leetcode.thirtydaysofcode.weekone

//Given an integer array nums, find the contiguous subarray (containing at least one number)
// which has the largest sum and return its sum.
//
//Example:
//
//Input: [-2,1,-3,4,-1,2,1,-5,4],
//Output: 6
//Explanation: [4,-1,2,1] has the largest sum = 6.
//Follow up:
//
//If you have figured out the O(n) solution,
// try coding another solution using the divide and conquer approach, which is more subtle.

object MaximumSubarray {

  def maxSubArray(nums: Array[Int]): Int =
    nums
      .drop(1)
      .foldLeft((nums(0), nums(0))) { (result, nextInt) =>
        //keep track of current total + max so far
        val (currentTotal, maximum) = result
        val nextTotal = Math.max(currentTotal + nextInt, nextInt)
        val nextMaximum = Math.max(maximum, nextTotal)
        (nextTotal, nextMaximum)
      }._2


}
