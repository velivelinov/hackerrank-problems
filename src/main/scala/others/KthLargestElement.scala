package others

import scala.collection.mutable

//Find the kth largest element in an unsorted array. Note that it is the kth largest element in the sorted order,
// not the kth distinct element.
//
//Example 1:
//
//Input: [3,2,1,5,6,4] and k = 2
//Output: 5
//Example 2:
//
//Input: [3,2,3,1,2,4,5,5,6] and k = 4
//Output: 4
//Note:
//You may assume k is always valid, 1 ≤ k ≤ array's length.
object KthLargestElement extends App {

  def findKthLargest(nums: Array[Int], k: Int): Int = {

    val minHeap = mutable.PriorityQueue.empty(Ordering[Int].reverse)
    nums
      .foreach { nextNum =>
        minHeap.enqueue(nextNum)
        if (minHeap.size > k) {
          minHeap.dequeue
        }
      }

    minHeap.dequeue
  }

  println(findKthLargest(Array(1, 22, 3, 863, 58, -1, 5), 2))
}