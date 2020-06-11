package others.recursionanddp

object Factorial extends App {

  def factorial(n: Int): Int = {

    @scala.annotation.tailrec
    def factorialHelper(n: Int, product: Int): Int =
      n match {
        case 0 => product
        case _ => factorialHelper(n - 1, product * n)
      }

    factorialHelper(n, 1)
  }

}
