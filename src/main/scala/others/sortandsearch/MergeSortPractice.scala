package others.sortandsearch


object MergeSortPractice extends App {

  @scala.annotation.tailrec
  def merge(left: List[Int], right: List[Int], res: List[Int] = Nil): List[Int] =
    (left, right) match {
      case (Nil, Nil) => Nil
      case (left, Nil) => res ++ left
      case (Nil, right) => res ++ right
      case (lHead :: lTail, rHead :: rTail) =>
        if (lHead < rHead) merge(lTail, right, res :+ lHead)
        else merge(left, rTail, res :+ rHead)
    }

  def mergeSort(list: List[Int]): List[Int] = {
    val midPoint = list.length / 2
    if (midPoint == 0) list
    else {
      val (left, right) = list.splitAt(midPoint)
      merge(mergeSort(left), mergeSort(right))
    }
  }

  println(mergeSort(List(10, 5, 525, 1, -1, 5, -12, -4)))
}
