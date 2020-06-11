package leetcode

//Given an array of integers, return indices of the two numbers such that they add up to a specific target.
//
//You may assume that each input would have exactly one solution, and you may not use the same element twice.
//
//Example:
//
//Given nums = [2, 7, 11, 15], target = 9,
//
//Because nums[0] + nums[1] = 2 + 7 = 9,return [0, 1].
object TwoSum extends App {

  def twoSum(nums: List[Int], target: Int): List[Int] = {

    def helper(processed: Map[Int, Int], next: List[Int], target: Int, currentIndex: Int): List[Int] = {
      next match {
        case Nil => List.empty[Int]
        case head :: _ if processed.contains(target - head) => List(processed(target - head), currentIndex)
        case head :: tail =>
          helper(processed.+((head, currentIndex)), tail, target, currentIndex + 1)
      }
    }

    helper(Map.empty[Int, Int], nums, target, 0)
  }

  println(twoSum(List(2, 7, 11, 15), 9))
}

object Solution extends App {
  def decompressRLElist(nums: Array[Int]): Array[Int] = {

    def helper(numList: List[Int], output: String): String = {
      numList match {
        case Nil => output
        case first :: second :: tail =>
          helper(tail, output + (second.toString * first))
      }
    }

    helper(nums.toList, "").toList.map(_.asDigit).toArray
  }

  println(decompressRLElist(Array(1, 2, 3, 4)).toList)
}