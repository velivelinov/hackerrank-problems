package warmup

import scala.collection.immutable.HashSet

object SockMerchant extends App {
    // Complete the sockMerchant function below.
    def sockMerchant(n: Int, list: List[Int]): Int = {
      list.groupBy(identity).map(_._2.length / 2).sum
    }

    def sockMerchantRecursive(n: Int, list: List[Int]): Int = {

      def recurse(list: List[Int], counter: Int, unpaired: HashSet[Int]): Int =
        list match {
          case Nil => counter
          case head :: tail if unpaired.contains(head) => recurse(tail, counter + 1, unpaired - head)
          case head :: tail  => recurse(tail, counter, unpaired + head)
        }

      recurse(list, 0, HashSet.empty[Int])
    }
}