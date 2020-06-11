package others

object AllPermutations extends App {

  // a - a
  // a,b - ab, ba
  // a,b,c, - abc, acb, bac, bca, cba, cab

  def allPermutations(input: String): List[String] = {

    def insertEverywhere(toInsert: Char, word: List[Char]): List[List[Char]] =
      (0 to word.length).map { index =>
        (word.slice(0, index) :+ toInsert) ++ word.slice(index, word.length)
      }.toList

    def permutationsHelper(input: List[Char], soFar: List[List[Char]]): List[List[Char]] =
      input match {
        case Nil => soFar
        case head :: tail if soFar.isEmpty =>
          permutationsHelper(tail, List(List(head)))
        case head :: tail =>
          permutationsHelper(tail, soFar.flatMap(word => insertEverywhere(head, word)))
      }

    permutationsHelper(input.toList, Nil).map(list => list.mkString)
  }

  println(allPermutations("abc"))
}
