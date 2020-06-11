package others

object SumLists extends App {

  def addTwoNumbers(l1: List[Int], l2: List[Int]): List[Int] = {
    (l1, l2) match {
      case (a :: as, b :: bs) =>
        val digit = (a + b) % 10
        val carry = (a + b) / 10
        if (carry > 0) digit :: addTwoNumbers(addTwoNumbers(as, carry :: Nil), bs)
        else digit :: addTwoNumbers(as, bs)
      case (as, Nil)  => as
      case (Nil, bs) => bs
    }
  }

    println(addTwoNumbers(List(1, 0, 9), List(2, 1, 2)))
}
