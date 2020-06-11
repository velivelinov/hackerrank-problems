package leetcode


//Given a string containing just the characters '(', ')', '{', '}', '[' and ']', determine if the input string is valid.
//
//An input string is valid if:
//
//Open brackets must be closed by the same type of brackets.
//Open brackets must be closed in the correct order.
//Note that an empty string is also considered valid.

object ValidParentheses {

  val brackets = Map(
    '(' -> ')',
    '{' -> '}',
    '['-> ']'
  )

  def isValid(s: String): Boolean = {
    @scala.annotation.tailrec
    def helper(input: List[Char], soFar: List[Char]): Boolean =
      input match {
        case Nil if soFar.isEmpty => true
        case Nil => false
        case head :: tail if brackets.contains(head) => helper(tail, brackets(head) :: soFar)
        case head :: tail if soFar.headOption == Some(head) => helper(tail, soFar.tail)
        case _ =>  false
      }

    helper(s.toList, List.empty[Char])
  }
}
