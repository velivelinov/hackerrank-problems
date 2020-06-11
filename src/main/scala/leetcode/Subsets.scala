package leetcode

//Given a set of distinct integers, nums, return all possible subsets (the power set).
//
//Note: The solution set must not contain duplicate subsets.
//
//Example:
//
//Input: nums = [1,2,3]
//Output:
//[
//[3],
//[1],
//[2],
//[1,2,3],
//[1,3],
//[2,3],
//[1,2],
//[]
//]
object Subsets extends App {

  def subsets(nums: Array[Int]): List[List[Int]] = {

    nums
      .foldLeft(List(List.empty[Int])) { (soFar, nextElement) =>
        soFar ++ soFar.map(_ :+ nextElement)
      }
  }
}
