package leetcode

//Suppose an array sorted in ascending order is rotated at some pivot unknown to you beforehand.
//
//(i.e., [0,1,2,4,5,6,7] might become [4,5,6,7,0,1,2]).
//
//You are given a target value to search. If found in the array return its index, otherwise return -1.
//
//You may assume no duplicate exists in the array.
//
//Your algorithm's runtime complexity must be in the order of O(log n).
//
//Example 1:
//
//Input: nums = [4,5,6,7,0,1,2], target = 0
//Output: 4
//Example 2:
//
//Input: nums = [4,5,6,7,0,1,2], target = 3
//Output: -1

object SearchInRotatedArray extends App {
  def search(nums: Array[Int], target: Int): Int = {

    def belongs(el: Int, from: Int, to: Int) =
      from <= el && el <= to

    @scala.annotation.tailrec
    def searchHelper(el: Int, list: Array[Int], from: Int, to: Int): Int = {
      if (from > to) -1
      else {
        val mid = (from + to) / 2
        if (list(mid) == el) mid
        else if (
          (list(from) < list(mid) && belongs(el, list(from), list(mid))) ||
            (list(mid) < list(to) && !belongs(el, list(mid), list(to)))
        ) {
          searchHelper(el, list, from, mid - 1)
      } else {
          searchHelper(el, list, mid + 1, to)
        }
      }
    }

    if (nums.isEmpty) -1 else searchHelper(target, nums, 0, nums.length - 1)
  }
}
