package others.sortandsearch

object BinarySearch extends App {

  def binarySearch(el: Int, list: List[Int]): Int = {

    @scala.annotation.tailrec
    def binarySearchHelper(el: Int, list: List[Int], from: Int, to: Int): Int = {
      if (from > to) -1
      else {
        val mid = from + (to - from) / 2
        list(mid) match {
          case x if x == el => mid
          case x if x > el => binarySearchHelper(el, list, from, mid - 1)
          case x if x < el => binarySearchHelper(el, list, mid + 1, to)
        }
      }
    }

    binarySearchHelper(el, list, 0, list.length - 1)
  }

  println(binarySearch(3, List(1, 2, 3, 4, 5, 6)))
  println(binarySearch(0, List(1, 2, 3, 4, 5, 6)))
}
