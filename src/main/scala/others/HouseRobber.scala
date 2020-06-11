package others

//You are a professional robber planning to rob houses along a street.
//Each house has a certain amount of money stashed, the only constraint stopping you from robbing each of them is
//that adjacent houses have security system connected and it will automatically contact the police if
//two adjacent houses were broken into on the same night.
//
//Given a list of non-negative integers representing the amount of money of each house,
//determine the maximum amount of money you can rob tonight without alerting the police.
//
//Example 1:
//
//Input: [1,2,3,1]
//Output: 4
//Explanation: Rob house 1 (money = 1) and then rob house 3 (money = 3).
//Total amount you can rob = 1 + 3 = 4.
//Example 2:
//
//Input: [2,7,9,3,1]
//Output: 12
//Explanation: Rob house 1 (money = 2), rob house 3 (money = 9) and rob house 5 (money = 1).
//Total amount you can rob = 2 + 9 + 1 = 12.
object HouseRobber extends App {

  def rob(nums: Array[Int]): Int = {


    def helper(nums: List[Int], result: Int): Int =
      nums match {
        case Nil => result
        case List(lastHouse) => result + lastHouse
        case h :: h2 :: tail =>
          Math.max(helper(h2 :: tail, result), helper(tail, result + h))
      }

    helper(nums.toList, 0)
  }

  def rob2(nums: Array[Int]): Int = {
    // handle edge case of empty array
    if (nums.isEmpty) 0
    else {
      // represents best payload when robbing up to and including this house
      // Ex. best(3) means best payload from House 1, House 2, House 3, and House 4
      val best: Array[Int] = Array.fill(nums.length)(0)

      for (i <- nums.indices) {
        best(i) =
          if (i == 0) nums(0) // only one house, rob first house
          else if (i == 1) nums(0) max nums(1) // only two houses, compare them
          else nums(i) + best(i-2) max best(i-1)
      }

      best.last
    }
  }


  def minCostClimbingStairs(cost: Array[Int]): Int = {
    // cost at step i = Min(cost(step + 1), cost(step + 2))
    if (cost.isEmpty) 0
    else {
      val cheapest = Array.fill(cost.length)(0)

      for (level <- cost.indices) {
        cheapest(level) =
          if (level == 0) cost(0)
          else if (level == 1) Math.min(cost(0), cost(1))
          else cost(level) + Math.min(cheapest(level-1), cheapest(level-2))
      }

      cheapest.foreach(println)
      Math.min(cheapest(cost.length - 1), cheapest(cost.length - 2))
    }
  }

  println(minCostClimbingStairs(Array(0, 1, 1, 0)))
}
