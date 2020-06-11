package leetcode.thirtydaysofcode.weekone

import scala.collection.immutable.HashSet

//Write an algorithm to determine if a number is "happy".
//
//A happy number is a number defined by the following process: Starting with any positive integer, replace the number by the sum of the squares of its digits, and repeat the process until the number equals 1 (where it will stay), or it loops endlessly in a cycle which does not include 1. Those numbers for which this process ends in 1 are happy numbers.
//
//Example:
//
//Input: 19
//Output: true
//Explanation:
//1^2 + 9^2 = 82
//8^2 + 2^2 = 68
//6^2 + 8^2 = 100
//1^2 + 0^2 + 02 = 1

object HappyNumber {

  def isHappy(n: Int): Boolean = {

    @annotation.tailrec
    def squaredSum(n: Int, sum: Int): Int =
      n match {
        case 0 => sum
        case _ => squaredSum(n/10, sum + (n % 10) * (n % 10))
      }

    @annotation.tailrec
    def solve(next: Int, soFar: HashSet[Int]): Boolean =
      next match {
        case 1 => true
        case n if soFar.contains(n) => false
        case n => solve(squaredSum(n, 0), soFar+n)
      }

    solve(n, HashSet.empty[Int])
  }

}
