package others.sortandsearch

object MergeSort extends App {

  @scala.annotation.tailrec
  def mergeTailrec(left: List[Int], right: List[Int], res: List[Int] = List.empty[Int]): List[Int] =
    (left, right) match {
    case (_, Nil) => res ++ left
    case (Nil, _) => res ++ right
    case (leftHead :: leftTail, rightHead :: rightTail) =>
      if (leftHead < rightHead) mergeTailrec(leftTail, right, res :+ leftHead)
      else mergeTailrec(left, rightTail, res :+ rightHead)
  }

  def merge(left: List[Int], right: List[Int]): List[Int] =
    (left, right) match {
      case (_, Nil) => left
      case (Nil, _) => right
      case (leftHead :: leftTail, rightHead :: rightTail) =>
        if (leftHead < rightHead) leftHead :: merge(leftTail, right)
        else rightHead :: merge(left, rightTail)
    }

  def mergeSort(list: List[Int]): List[Int] = {
    val midPoint = list.length / 2
    if (midPoint == 0) list // either empty or single-value list - already sorted
    else {
      val (left, right) = list.splitAt(midPoint)
      mergeTailrec(mergeSort(left), mergeSort(right))
    }
  }

  println(mergeSort(List(10, 5, 525, 1, -1, 5, -12, -4)))
}
