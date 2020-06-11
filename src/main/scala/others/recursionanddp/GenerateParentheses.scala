package others.recursionanddp

//Given n pairs of parentheses, write a function to generate all combinations of well-formed parentheses.
//
//For example, given n = 3, a solution set is:
//
//[
//"((()))",
//"(()())",
//"(())()",
//"()(())",
//"()()()"
//]
object GenerateParentheses extends App {

  def generateParentheses(n: Int): List[String] = {

    def generateParenthesisHelper(left: Int, right: Int, acc: String): List[String] =
      if (left == 0 && right == 0) List(acc)
      else if (left < 0 || right < 0 || left < right) List.empty[String]
      else
        generateParenthesisHelper(left - 1, right, s"($acc") ++
          generateParenthesisHelper(left, right - 1, s")$acc")

    generateParenthesisHelper(n, n, "")
  }
}
