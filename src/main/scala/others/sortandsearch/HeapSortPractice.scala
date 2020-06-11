package others.sortandsearch

object HeapSortPractice extends App {

  def swap(array: Array[Int], index: Int, newIndex: Int): Unit = {
    val temp = array(index)
    array(index) = array(newIndex)
    array(newIndex) = temp
  }

  def bubbleUpElement(array: Array[Int], index: Int, size: Int): Unit = {
    val left = (2 * index) + 1
    val right = left + 1
    var newIndex = -1
    newIndex = if(left < size && array(left) > array(index)) left else index
    newIndex = if(right < size && array(right) > array(newIndex)) right else newIndex
    if (index != newIndex) {
      swap(array, index, newIndex)
      if (newIndex * 2 < size) bubbleUpElement(array, newIndex, size)
    }
  }

  def buildHeap(array: Array[Int], size: Int): Unit = {
    val halfPoint = size / 2
    for (index <- (0 to halfPoint).reverse) {
      bubbleUpElement(array, index, size)
    }
  }

  def heapSort(array: Array[Int]): Unit = {
    buildHeap(array, array.length)
    for(index <- array.indices.reverse) {
      swap(array, 0, index)
      bubbleUpElement(array, 0, index)
    }
  }

  val data = Array (1,0,2,9,3,8)//,4,7,5,6)
  heapSort(data)
  data.foreach(println)

}
