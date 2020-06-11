package leetcode.thirtydaysofcode.weekthree


//Given a string containing only three types of characters: '(', ')' and '*',
// write a function to check whether this string is valid. We define the validity of a string by these rules:
//
//Any left parenthesis '(' must have a corresponding right parenthesis ')'.
//Any right parenthesis ')' must have a corresponding left parenthesis '('.
//Left parenthesis '(' must go before the corresponding right parenthesis ')'.
//'*' could be treated as a single right parenthesis ')' or a single left parenthesis '(' or an empty string.
//An empty string is also valid.
//Example 1:
//Input: "()"
//Output: True
//Example 2:
//Input: "(*)"
//Output: True
//Example 3:
//Input: "(*))"
//Output: True
//Note:
//The string size will be in the range [1, 100].

object ValidParentheses extends App {

  def checkValidString(s: String): Boolean = {

    def helper(input: List[Char], leftCount: Int): Boolean =
      input match {
        case Nil if leftCount > 0 => false
        case Nil => true
        case '(' :: tail => helper(tail, leftCount + 1)
        case ')' :: tail if leftCount > 0 => helper(tail, leftCount - 1)
        case ')' :: tail => false
        case '*' :: tail =>
          helper(tail, leftCount + 1) ||
            helper(tail, leftCount) ||
              helper(tail, leftCount - 1)
      }

    helper(s.toList, 0)
  }

  def checkValidString2(s: String): Boolean = {

    s.foldLeft((0,0)) { (balance, nextChar) =>
      val (low, high) = balance
      val newLow = Math.max(if (nextChar == '(') low + 1 else low - 1, 0)
      val newHigh = if (nextChar != ')') high + 1 else high - 1
      if (newHigh < 0) return false
      else (newLow, newHigh)
    }._1 == 0
  }

}
