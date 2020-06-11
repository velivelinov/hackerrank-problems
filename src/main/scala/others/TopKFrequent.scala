package others

//Given a non-empty array of integers, return the k most frequent elements.
//
//Example 1:
//
//Input: nums = [1,1,1,2,2,3], k = 2
//Output: [1,2]
//Example 2:
//
//Input: nums = [1], k = 1
//Output: [1]
object TopKFrequent extends App {

  case class IntCount(int: Int, count: Int)

  def topKFrequent(nums: Array[Int], k: Int): Array[Int] = {

    val numsGrouped = nums.groupBy(identity).mapValues(_.length).map{case (k, v) => IntCount(k, v)}

    val orderingByCount: Ordering[IntCount] = Ordering.by(i => i.count)

    val minHeap = scala.collection.mutable.PriorityQueue.empty[IntCount](orderingByCount.reverse)

    numsGrouped
      .foreach { nextNum =>
        minHeap.enqueue(nextNum)
        if (minHeap.size > k) {
          minHeap.dequeue
        }
      }

    minHeap.toArray.map(_.int)
  }

  println(topKFrequent(Array(4,1,-1,2,-1,2,3), 2).toList)
}
