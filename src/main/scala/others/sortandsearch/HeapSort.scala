package others.sortandsearch

object HeapSort extends App {

    def swap(a: Array[Int], i: Int, j: Int): Unit = {
      val t = a(i)
      a(i) = a(j)
      a(j) = t
    }

    def maxHeap (array: Array[Int], index: Int, size: Int): Unit = {
      val left = (2 * index) + 1
      val right = left + 1
      var newIndex = -1
      newIndex = if (left < size && array(left) > array(index)) left else index
      newIndex = if (right < size && array(right) > array(newIndex)) right else newIndex
      if (newIndex != index) {
        swap(array, index, newIndex)
        if (newIndex * 2 < size) maxHeap(array, newIndex, size)
        array.foreach(print)
        println(" ")
      }
    }

    def buildMaxHeap (array: Array[Int], size: Int): Unit = {
      val halfPoint = size / 2
      for (index <- 0 to halfPoint) {
        maxHeap(array, halfPoint - index, size)
      }
    }

    def heapSort (a: Array[Int]) {
      buildMaxHeap(a, a.length)
//      for (i <- a.indices.reverse) {
//        swap(a, 0, i)
//        maxHeap(a, 0, i)
//      }
    }

    val data = Array (1,0,2,9,3,8)//,4,7,5,6)
    heapSort(data)
    data.foreach(println)

}
