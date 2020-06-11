package others.recursionanddp

object Fibonacci extends App {

  def fibonacci(n: Int): Int = {

    @scala.annotation.tailrec
    def fibHelper(n: Int, a: Int, b: Int): Int =
      n match {
        case 0 => a
        case _ => fibHelper(n-1, b, a+b)
      }

    fibHelper(n, 0, 1)
  }

  println(fibonacci(0))
  println(fibonacci(1))
}
