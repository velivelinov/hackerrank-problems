package others

import scala.collection.immutable.HashSet

object PermutationOfPalindrome extends App {

  def isPermutationOfPalindrome(input: String): Boolean =
    input
      .replaceAll(" ", "")
      .toLowerCase
      .toList
      .foldLeft(HashSet.empty[Char]){(soFar, next) =>
        if (soFar.contains(next)) soFar - next
        else soFar + next
      }
      .size < 2

  println(s"taco cat is: ${isPermutationOfPalindrome("taco cat")}")
  println(s"cat taco is: ${isPermutationOfPalindrome("cat taco")}")
  println(s"tactic is: ${isPermutationOfPalindrome("tactic")}")
}
