package others

object StringCompression extends App {

  case class CharCount(char: Char, count: Int)

  def stringCompression(input: String): String = {

    def helper(currentChar: Char, currentCharCount: Int, toProcess: List[Char], soFar: List[CharCount]): List[CharCount] =
      toProcess match {
        case Nil => soFar :+ CharCount(currentChar, currentCharCount)
        case head :: tail if currentChar == head =>
          helper(currentChar, currentCharCount + 1, tail, soFar)
        case head :: tail =>
          helper(head, 1, tail, soFar :+ CharCount(currentChar, currentCharCount))
      }

    input.toList match {
      case Nil => ""
      case head :: tail =>
        val compressed = helper(head, 1, tail, List.empty[CharCount])
          .foldLeft(""){(soFar, nextCharCount) =>
            soFar + nextCharCount.char + nextCharCount.count
          }
        if (compressed.length < input.length) compressed else input
    }
  }

  def stringCompressionFold(input: String): String = {
    if (input.size > 1)
      input
      .toList
      .drop(1)
      .foldLeft((List.empty[Char], CharCount(input.head, 1))) { (results, next) =>
        val (output, currentCharCount) = results
        if (next == currentCharCount.char) (output, currentCharCount.copy(count = currentCharCount.count + 1))
        else (output :+ currentCharCount.count.toChar :+ next, CharCount(next, 1))
      }
      ._1.mkString
    else ""
  }

  println(s"compressing aaaaa is ${stringCompression("aaaaa")}")
  println(s"compressing (empty) is ${stringCompression("")}")
  println(s"compressing aabc is ${stringCompression("aabc")}")

  println(s"compressing aaaaa is ${stringCompressionFold("aaaaa")}")
  println(s"compressing (empty) is ${stringCompressionFold("")}")
  println(s"compressing aabc is ${stringCompressionFold("aabc")}")

}
