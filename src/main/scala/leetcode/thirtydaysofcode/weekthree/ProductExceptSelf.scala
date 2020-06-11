package leetcode.thirtydaysofcode.weekthree

//Given an array nums of n integers where n > 1,
// return an array output such that output[i] is equal to the product of all the elements of nums except nums[i].
//
//Example:
//
//Input:  [1,2,3,4]
//Output: [24,12,8,6]
//Constraint: It's guaranteed that the product of the elements of any prefix or suffix of the
// array (including the whole array) fits in a 32 bit integer.
//
//Note: Please solve it without division and in O(n).
//
//Follow up:
//Could you solve it with constant space complexity?
//(The output array does not count as extra space for the purpose of space complexity analysis.)

object ProductExceptSelf extends App {

  def productExceptSelf(nums: Array[Int]): Array[Int] = {

    val productsLeft = nums.init
      .foldLeft((List(1), 1)) { (currentState, current) =>
        val (listSoFar, previousEl) = currentState
        (listSoFar :+ (previousEl * current), (previousEl * current))
      }._1

    val productsRight = nums
      .tail
      .foldRight((List(1), 1)) { (current, currentState) =>
        val (listSoFar, previousEl) = currentState
        ((previousEl * current) :: listSoFar, (previousEl * current))
      }._1

    nums.foldLeft((List.empty[Int], 0)) { (currentState, _) =>
      val (soFar, index) = currentState
      (soFar :+ (productsLeft(index) * productsRight(index)), index + 1)
    }._1.toArray
  }

  println(productExceptSelf(Array(2,3,4,5)))
}
